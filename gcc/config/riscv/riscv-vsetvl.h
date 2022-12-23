/* VSETVL pass header for RISC-V 'V' Extension for GNU compiler.
   Copyright(C) 2022-2022 Free Software Foundation, Inc.
   Contributed by Juzhe Zhong (juzhe.zhong@rivai.ai), RiVAI Technologies Ltd.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or(at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef GCC_RISCV_VSETVL_H
#define GCC_RISCV_VSETVL_H

#define IS_AGNOSTIC(VALUE) (bool) (VALUE & 0x1 || (VALUE >> 1 & 0x1))

namespace riscv_vector {

/* Classification of vsetvl instruction.  */
enum vsetvl_type
{
  VSETVL_NORMAL,
  VSETVL_VTYPE_CHANGE_ONLY,
  VSETVL_DISCARD_RESULT,
};

enum emit_type
{
  /* emit_insn directly.  */
  EMIT_DIRECT,
  EMIT_BEFORE,
  EMIT_AFTER,
};

enum demand_type
{
  DEMAND_AVL,
  DEMAND_SEW,
  DEMAND_LMUL,
  DEMAND_RATIO,
  DEMAND_TAIL_POLICY,
  DEMAND_MASK_POLICY,
  NUM_DEMAND
};

/* AVL info for RVV instruction. Most RVV instructions have AVL operand in
   implicit dependency. The AVL comparison between 2 RVV instructions is
   very important since it affects our decision whether we should insert
   a vsetvl instruction in this situation. AVL operand of all RVV instructions
   can only be either a const_int value with < 32 or a reg value which can be
   define by either a real RTL instruction or a PHI instruction. So we need a
   standalone method to define AVL comparison and we can not simpily use
   operator "==" to compare 2 RTX value since it's to strict which will make
   use miss a lot of optimization opportunities. This method handle these
   following cases:

     -  Background:
	  Insert-vsetvl PASS is working after RA.

     -  Terminology:
	  - pr: Pseudo-register.
	  - hr: Hardware-register.

     -  Case 1:

	Before RA:
	  li pr138,13
	  insn1 (implicit depend on pr138).
	  li pr138,14
	  insn2 (implicit depend on pr139).

	After RA:
	  li hr5,13
	  insn1 (implicit depend on hr5).
	  li hr5,14
	  insn2 (implicit depend on hr5).

	Correct IR after vsetvl PASS:
	  li hr5,13
	  vsetvl1 zero,hr5....
	  insn1 (implicit depend on hr5).
	  li hr5,14
	  vsetvl2 zero,hr5....
	  insn2 (implicit depend on hr5).

     In this case, both insn1 and insn2 are using hr5 as the same AVL.
     If we use "rtx_equal_p" or "REGNO (AVL1) == REGNO (AVL)", we will end
     up with missing the vsetvl2 instruction which creates wrong result.

     Note: Using "==" operator to compare 2 AVL RTX strictly can fix this
     issue. However, it is a too strict comparison method since not all member
     variables in RTX data structure are not neccessary to be the same. It will
     make us miss a lot of optimization opportunities.

     -  Case 2:

	After RA:
	bb 0:
	  li hr5,13
	bb 1:
	  li hr5,14
	bb2:
	  insn1 (implicit depend on hr5).
	  insn2 (implicit depend on hr5).

     In this case, we may end up with different AVL RTX and produce redundant
     vsetvl instruction.

     VALUE is the implicit dependency in each RVV instruction.
     SOURCE is the source definition information of AVL operand.  */
class avl_info
{
private:
  rtx m_value;
  rtl_ssa::set_info *m_source;

public:
  avl_info () : m_value (NULL_RTX), m_source (nullptr) {}
  avl_info (const avl_info &);
  avl_info (rtx, rtl_ssa::set_info *);
  rtx get_value () const { return m_value; }
  rtl_ssa::set_info *get_source () const { return m_source; }
  avl_info &operator= (const avl_info &);
  bool operator== (const avl_info &) const;
  bool operator!= (const avl_info &) const;
};

/* Basic structure to save VL/VTYPE information.  */
struct vl_vtype_info
{
protected:
  /* AVL can be either register or const_int.  */
  avl_info m_avl;
  /* Fields from VTYPE. The VTYPE checking depend on the flag
     dem_* before.  */
  uint8_t m_sew;
  riscv_vector::vlmul_type m_vlmul;
  uint8_t m_ratio;
  bool m_ta;
  bool m_ma;

public:
  void set_sew (uint8_t sew) { m_sew = sew; }
  void set_vlmul (riscv_vector::vlmul_type vlmul) { m_vlmul = vlmul; }
  void set_ratio (uint8_t ratio) { m_ratio = ratio; }
  void set_ta (bool ta) { m_ta = ta; }
  void set_ma (bool ma) { m_ma = ma; }

  vl_vtype_info ()
    : m_avl (avl_info ()), m_sew (0), m_vlmul (riscv_vector::LMUL_RESERVED),
      m_ratio (0), m_ta (0), m_ma (0)
  {}
  vl_vtype_info (const vl_vtype_info &) = default;
  vl_vtype_info &operator= (const vl_vtype_info &) = default;
  vl_vtype_info (avl_info, uint8_t, riscv_vector::vlmul_type, uint8_t, bool,
		 bool);

  bool operator== (const vl_vtype_info &) const;
  bool operator!= (const vl_vtype_info &) const;

  bool has_avl_imm () const { return get_avl () && CONST_INT_P (get_avl ()); }
  bool has_avl_reg () const { return get_avl () && REG_P (get_avl ()); }
  bool has_avl_no_reg () const { return !get_avl (); }
  bool has_non_zero_avl () const;

  rtx get_avl () const { return m_avl.get_value (); }
  avl_info get_avl_info () const { return m_avl; }
  void set_avl_info (const avl_info &avl) { m_avl = avl; }
  uint8_t get_sew () const { return m_sew; }
  riscv_vector::vlmul_type get_vlmul () const { return m_vlmul; }
  uint8_t get_ratio () const { return m_ratio; }
  bool get_ta () const { return m_ta; }
  bool get_ma () const { return m_ma; }

  bool same_avl_p (const vl_vtype_info &) const;
  bool same_vtype_p (const vl_vtype_info &) const;
  bool same_vlmax_p (const vl_vtype_info &) const;
};

class vector_insn_info : public vl_vtype_info
{
private:
  enum state_type
  {
    UNINITIALIZED,
    VALID,
    UNKNOWN,
    EMPTY,

    /* The block is polluted as containing VSETVL instruction during dem
       backward propagation to gain better LCM optimization even though
       such VSETVL instruction is not really emit yet during this time.  */
    DIRTY
  };

  enum state_type m_state;

  bool m_demands[NUM_DEMAND];

  /* TODO: Assume INSN1 = INSN holding of definition of AVL.
		  INSN2 = INSN that is inserted a vsetvl insn before.
     We may need to add a new member to save INSN of holding AVL.
     m_insn is holding the INSN that is inserted a vsetvl insn before in
     Phase 2. Ideally, most of the time INSN1 == INSN2. However, considering
     such case:

	vmv.x.s (INSN2)
	vle8.v (INSN1)

     If these 2 instructions are compatible, we should only issue a vsetvl INSN
     (with AVL included) before vmv.x.s, but vmv.x.s is not the INSN holding the
     definition of AVL.  */
  rtl_ssa::insn_info *m_insn;
  /* Save instruction pattern for Dirty block.
     Since empty block may be polluted as a dirty block during dem backward
     propagation (phase 3) which is intending to cheat LCM there is a VSETVL
     instruction here to gain better LCM optimization. Such instruction is not
     emit yet, we save this here and then emit it in the 4th phase if it is
     necessary.  */
  rtx m_dirty_pat;

  /* Parse the instruction to get VL/VTYPE information and demanding
   * information.  */
  /* This is only called by simple_vsetvl subroutine when optimize == 0.
     Since RTL_SSA can not be enabled when optimize == 0, we don't initialize
     the m_insn.  */
  void parse_insn (rtx_insn *);
  /* This is only called by lazy_vsetvl subroutine when optimize > 0.
     We use RTL_SSA framework to initialize the insn_info.  */
  void parse_insn (rtl_ssa::insn_info *);

  friend class vector_infos_manager;

public:
  vector_insn_info ()
    : vl_vtype_info (), m_state (UNINITIALIZED), m_demands{false},
      m_insn (nullptr), m_dirty_pat (NULL_RTX)
  {}

  bool operator> (const vector_insn_info &) const;
  bool operator>= (const vector_insn_info &) const;
  bool operator== (const vector_insn_info &) const;

  bool uninit_p () const { return m_state == UNINITIALIZED; }
  bool valid_p () const { return m_state == VALID; }
  bool unknown_p () const { return m_state == UNKNOWN; }
  bool empty_p () const { return m_state == EMPTY; }
  bool dirty_p () const { return m_state == DIRTY; }
  bool valid_or_dirty_p () const
  {
    return m_state == VALID || m_state == DIRTY;
  }

  static vector_insn_info get_unknown ()
  {
    vector_insn_info info;
    info.set_unknown ();
    return info;
  }

  void set_valid () { m_state = VALID; }
  void set_unknown () { m_state = UNKNOWN; }
  void set_empty () { m_state = EMPTY; }
  void set_dirty () { m_state = DIRTY; }
  void set_dirty_pat (rtx pat) { m_dirty_pat = pat; }
  void set_insn (rtl_ssa::insn_info *insn) { m_insn = insn; }

  bool demand_p (enum demand_type type) const { return m_demands[type]; }
  void demand (enum demand_type type) { m_demands[type] = true; }
  void demand_vl_vtype ();
  void undemand (enum demand_type type) { m_demands[type] = false; }

  bool compatible_p (const vector_insn_info &) const;
  bool compatible_avl_p (const vl_vtype_info &) const;
  bool compatible_vtype_p (const vl_vtype_info &) const;
  bool compatible_p (const vl_vtype_info &) const;
  vector_insn_info merge (const vector_insn_info &, bool) const;

  rtl_ssa::insn_info *get_insn () const { return m_insn; }
  rtx get_dirty_pat () const { return m_dirty_pat; }

  void dump (FILE *) const;
};

struct vector_block_info
{
  /* The local_dem vector insn_info of the block.  */
  vector_insn_info local_dem;

  /* The reaching_out vector insn_info of the block.  */
  vector_insn_info reaching_out;

  vector_block_info () = default;
};

class vector_infos_manager
{
public:
  auto_vec<vector_insn_info> vector_insn_infos;
  auto_vec<vector_block_info> vector_block_infos;
  auto_vec<vector_insn_info *> vector_exprs;

  struct edge_list *vector_edge_list;
  sbitmap *vector_kill;
  sbitmap *vector_del;
  sbitmap *vector_insert;
  sbitmap *vector_antic;
  sbitmap *vector_transp;
  sbitmap *vector_comp;
  sbitmap *vector_avin;
  sbitmap *vector_avout;

  vector_infos_manager ();

  /* Create a new expr in expr list if it is not exist.  */
  void create_expr (vector_insn_info &);

  /* Get the expr id of the pair of expr.  */
  size_t get_expr_id (const vector_insn_info &) const;

  /* Return the number of expr that is set in the bitmap.  */
  size_t expr_set_num (sbitmap) const;

  /* Get all relaxer expression id for corresponding vector info.  */
  auto_vec<size_t> get_all_available_exprs (const vector_insn_info &) const;

  /* Return true if all expression set in bitmap are same ratio.  */
  bool all_same_ratio_p (sbitmap) const;

  void release (void);

  void dump (FILE *) const;
};

} // namespace riscv_vector
#endif
