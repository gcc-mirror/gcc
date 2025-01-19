/* Copyright (C) 1988-2025 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef GCC_I386_FEATURES_H
#define GCC_I386_FEATURES_H

enum xlogue_stub {
  XLOGUE_STUB_SAVE,
  XLOGUE_STUB_RESTORE,
  XLOGUE_STUB_RESTORE_TAIL,
  XLOGUE_STUB_SAVE_HFP,
  XLOGUE_STUB_RESTORE_HFP,
  XLOGUE_STUB_RESTORE_HFP_TAIL,

  XLOGUE_STUB_COUNT
};

enum xlogue_stub_sets {
  XLOGUE_SET_ALIGNED,
  XLOGUE_SET_ALIGNED_PLUS_8,
  XLOGUE_SET_HFP_ALIGNED_OR_REALIGN,
  XLOGUE_SET_HFP_ALIGNED_PLUS_8,

  XLOGUE_SET_COUNT
};

/* Register save/restore layout used by out-of-line stubs.  */
class xlogue_layout {
public:
  struct reginfo
  {
    unsigned regno;
    HOST_WIDE_INT offset;	/* Offset used by stub base pointer (rax or
				   rsi) to where each register is stored.  */
  };

  unsigned get_nregs () const			{return m_nregs;}
  HOST_WIDE_INT get_stack_align_off_in () const	{return m_stack_align_off_in;}

  const reginfo &get_reginfo (unsigned reg) const
  {
    gcc_assert (reg < m_nregs);
    return m_regs[reg];
  }

  static const char *get_stub_name (enum xlogue_stub stub,
				    unsigned n_extra_args);

  /* Returns an rtx for the stub's symbol based upon
       1.) the specified stub (save, restore or restore_ret) and
       2.) the value of cfun->machine->call_ms2sysv_extra_regs and
       3.) rather or not stack alignment is being performed.  */
  static rtx get_stub_rtx (enum xlogue_stub stub);

  /* Returns the amount of stack space (including padding) that the stub
     needs to store registers based upon data in the machine_function.  */
  HOST_WIDE_INT get_stack_space_used () const
  {
    const struct machine_function *m = cfun->machine;
    unsigned last_reg = m->call_ms2sysv_extra_regs + MIN_REGS - 1;

    gcc_assert (m->call_ms2sysv_extra_regs <= MAX_EXTRA_REGS);
    return m_regs[last_reg].offset + STUB_INDEX_OFFSET;
  }

  /* Returns the offset for the base pointer used by the stub.  */
  HOST_WIDE_INT get_stub_ptr_offset () const
  {
    return STUB_INDEX_OFFSET + m_stack_align_off_in;
  }

  static const class xlogue_layout &get_instance ();
  static unsigned count_stub_managed_regs ();
  static bool is_stub_managed_reg (unsigned regno, unsigned count);

  static const HOST_WIDE_INT STUB_INDEX_OFFSET = 0x70;
  static const unsigned MIN_REGS = NUM_X86_64_MS_CLOBBERED_REGS;
  static const unsigned MAX_REGS = 18;
  static const unsigned MAX_EXTRA_REGS = MAX_REGS - MIN_REGS;
  static const unsigned VARIANT_COUNT = MAX_EXTRA_REGS + 1;
  static const unsigned STUB_NAME_MAX_LEN = 20;
  static const char * const STUB_BASE_NAMES[XLOGUE_STUB_COUNT];
  static const unsigned REG_ORDER[MAX_REGS];
  static const unsigned REG_ORDER_REALIGN[MAX_REGS];

private:
  xlogue_layout ();
  xlogue_layout (HOST_WIDE_INT stack_align_off_in, bool hfp);
  xlogue_layout (const xlogue_layout &);

  /* True if hard frame pointer is used.  */
  bool m_hfp;

  /* Max number of register this layout manages.  */
  unsigned m_nregs;

  /* Incoming offset from 16-byte alignment.  */
  HOST_WIDE_INT m_stack_align_off_in;

  /* Register order and offsets.  */
  struct reginfo m_regs[MAX_REGS];

  /* Lazy-inited cache of symbol names for stubs.  */
  static char s_stub_names[2][XLOGUE_STUB_COUNT][VARIANT_COUNT]
			  [STUB_NAME_MAX_LEN];

  static const xlogue_layout s_instances[XLOGUE_SET_COUNT];
};

namespace {

class scalar_chain
{
 public:
  scalar_chain (enum machine_mode, enum machine_mode);
  virtual ~scalar_chain ();

  static unsigned max_id;

  /* Scalar mode.  */
  enum machine_mode smode;
  /* Vector mode.  */
  enum machine_mode vmode;

  /* ID of a chain.  */
  unsigned int chain_id;
  /* A queue of instructions to be included into a chain.  */
  bitmap queue;
  /* Instructions included into a chain.  */
  bitmap insns;
  /* All registers defined by a chain.  */
  bitmap defs;
  /* Registers used in both vector and sclar modes.  */
  bitmap defs_conv;

  /* Limit on chain discovery.  */
  unsigned max_visits;

  bitmap insns_conv;
  hash_map<rtx, rtx> defs_map;
  unsigned n_sse_to_integer;
  unsigned n_integer_to_sse;
  auto_vec<rtx_insn *> control_flow_insns;

  bool build (bitmap candidates, unsigned insn_uid, bitmap disallowed);
  virtual int compute_convert_gain () = 0;
  int convert ();

 protected:
  void add_to_queue (unsigned insn_uid);
  void emit_conversion_insns (rtx insns, rtx_insn *pos);
  rtx convert_compare (rtx op1, rtx op2, rtx_insn *insn);
  void mark_dual_mode_def (df_ref def);
  void convert_reg (rtx_insn *insn, rtx dst, rtx src);
  void convert_insn_common (rtx_insn *insn);
  void make_vector_copies (rtx_insn *, rtx);
  void convert_registers ();
  void convert_op (rtx *op, rtx_insn *insn);

 private:
  bool add_insn (bitmap candidates, unsigned insn_uid, bitmap disallowed);
  bool analyze_register_chain (bitmap candidates, df_ref ref,
			       bitmap disallowed);
  virtual void convert_insn (rtx_insn *insn) = 0;
};

class general_scalar_chain : public scalar_chain
{
 public:
  general_scalar_chain (enum machine_mode smode_, enum machine_mode vmode_)
    : scalar_chain (smode_, vmode_) {}
  int compute_convert_gain () final override;

 private:
  void convert_insn (rtx_insn *insn) final override;
  int vector_const_cost (rtx exp);
  rtx convert_rotate (enum rtx_code, rtx op0, rtx op1, rtx_insn *insn);
};

class timode_scalar_chain : public scalar_chain
{
 public:
  timode_scalar_chain () : scalar_chain (TImode, V1TImode) {}
  int compute_convert_gain () final override;

 private:
  void fix_debug_reg_uses (rtx reg);
  void convert_insn (rtx_insn *insn) final override;
};

} // anon namespace

bool ix86_save_reg (unsigned int regno, bool maybe_eh_return,
		    bool ignore_outlined);
int ix86_compare_version_priority (tree decl1, tree decl2);
tree ix86_generate_version_dispatcher_body (void *node_p);
tree ix86_get_function_versions_dispatcher (void *decl);
tree ix86_mangle_decl_assembler_name (tree decl, tree id);


#endif  /* GCC_I386_FEATURES_H */
