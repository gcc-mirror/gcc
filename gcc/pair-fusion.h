// Pass to fuse adjacent loads/stores into paired memory accesses.
//
// This file contains the definition of the virtual base class which is
// overriden by targets that make use of the pass.
//
// Copyright (C) 2023-2025 Free Software Foundation, Inc.
//
// This file is part of GCC.
//
// GCC is free software; you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// GCC is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with GCC; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

namespace rtl_ssa {
  class def_info;
  class insn_info;
  class insn_range_info;
  class bb_info;
}

// Information about a potential base candidate, used in try_fuse_pair.
// There may be zero, one, or two viable RTL bases for a given pair.
struct base_cand
{
  // DEF is the def of the base register to be used by the pair.
  rtl_ssa::def_info *def;

  // FROM_INSN is -1 if the base candidate is already shared by both
  // candidate insns.  Otherwise it holds the index of the insn from
  // which the base originated.
  //
  // In the case that the base is shared, either DEF is already used
  // by both candidate accesses, or both accesses see different versions
  // of the same regno, in which case DEF is the def consumed by the
  // first candidate access.
  int from_insn;

  // To form a pair, we do so by moving the first access down and the second
  // access up.  To determine where to form the pair, and whether or not
  // it is safe to form the pair, we track instructions which cannot be
  // re-ordered past due to either dataflow or alias hazards.
  //
  // Since we allow changing the base used by an access, the choice of
  // base can change which instructions act as re-ordering hazards for
  // this pair (due to different dataflow).  We store the initial
  // dataflow hazards for this choice of base candidate in HAZARDS.
  //
  // These hazards act as re-ordering barriers to each candidate insn
  // respectively, in program order.
  //
  // Later on, when we take alias analysis into account, we narrow
  // HAZARDS accordingly.
  rtl_ssa::insn_info *hazards[2];

  base_cand (rtl_ssa::def_info *def, int insn)
    : def (def), from_insn (insn), hazards {nullptr, nullptr} {}

  base_cand (rtl_ssa::def_info *def) : base_cand (def, -1) {}

  // Test if this base candidate is viable according to HAZARDS.
  bool viable () const;
};

struct alias_walker;

// When querying should_handle_writeback, this enum is used to
// qualify which opportunities we are asking about.
enum class writeback_type {
  // Only those writeback opportunities that arise from existing
  // auto-increment accesses.
  EXISTING,

  // All writeback opportunities, including those that involve folding
  // base register updates into a non-writeback pair.
  ALL
};

// This class can be overriden by targets to give a pass that fuses
// adjacent loads and stores into load/store pair instructions.
//
// The target can override the various virtual functions to customize
// the behaviour of the pass as appropriate for the target.
struct pair_fusion {
  pair_fusion ();

  // Given:
  // - an rtx REG_OP, the non-memory operand in a load/store insn,
  // - a machine_mode MEM_MODE, the mode of the MEM in that insn, and
  // - a boolean LOAD_P (true iff the insn is a load), then:
  // return true if the access should be considered an FP/SIMD access.
  // Such accesses are segregated from GPR accesses, since we only want
  // to form pairs for accesses that use the same register file.
  virtual bool fpsimd_op_p (rtx, machine_mode, bool)
  {
    return false;
  }

  // Return true if we should consider forming pairs from memory
  // accesses with operand mode MODE at this stage in compilation.
  virtual bool pair_operand_mode_ok_p (machine_mode mode) = 0;

  // Return true iff REG_OP is a suitable register operand for a paired
  // memory access, where LOAD_P is true if we're asking about loads and
  // false for stores.  MODE gives the mode of the operand.
  virtual bool pair_reg_operand_ok_p (bool load_p, rtx reg_op,
				      machine_mode mode) = 0;

  // Return alias check limit.
  // This is needed to avoid unbounded quadratic behaviour when
  // performing alias analysis.
  virtual int pair_mem_alias_check_limit () = 0;

  // Return true if we should try to handle writeback opportunities.
  // WHICH determines the kinds of writeback opportunities the caller
  // is asking about.
  virtual bool should_handle_writeback (writeback_type which) = 0;

  // Given BASE_MEM, the mem from the lower candidate access for a pair,
  // and LOAD_P (true if the access is a load), check if we should proceed
  // to form the pair given the target's code generation policy on
  // paired accesses.
  virtual bool pair_mem_ok_with_policy (rtx base_mem, bool load_p) = 0;

  // Generate the pattern for a paired access.  PATS gives the patterns
  // for the individual memory accesses (which by this point must share a
  // common base register).  If WRITEBACK is non-NULL, then this rtx
  // describes the update to the base register that should be performed by
  // the resulting insn.  LOAD_P is true iff the accesses are loads.
  virtual rtx gen_pair (rtx *pats, rtx writeback, bool load_p) = 0;

  // Return true if INSN is a paired memory access.  If so, set LOAD_P to
  // true iff INSN is a load pair.
  virtual bool pair_mem_insn_p (rtx_insn *insn, bool &load_p) = 0;

  // Return true if we should track loads.
  virtual bool track_loads_p ()
  {
    return true;
  }

  // Return true if we should track stores.
  virtual bool track_stores_p ()
  {
    return true;
  }

  // Return true if OFFSET is in range for a paired memory access.
  virtual bool pair_mem_in_range_p (HOST_WIDE_INT offset) = 0;

  // Given a load/store pair insn in PATTERN, unpack the insn, storing
  // the register operands in REGS, and returning the mem.  LOAD_P is
  // true for loads and false for stores.
  virtual rtx destructure_pair (rtx regs[2], rtx pattern, bool load_p) = 0;

  // Given a pair mem in MEM, register operands in REGS, and an rtx
  // representing the effect of writeback on the base register in WB_EFFECT,
  // return an insn representing a writeback variant of this pair.
  // LOAD_P is true iff the pair is a load.
  // This is used when promoting existing non-writeback pairs to writeback
  // variants.
  virtual rtx gen_promote_writeback_pair (rtx wb_effect, rtx mem,
					  rtx regs[2], bool load_p) = 0;

  void process_block (rtl_ssa::bb_info *bb);
  rtl_ssa::insn_info *find_trailing_add (rtl_ssa::insn_info *insns[2],
					 const rtl_ssa::insn_range_info
					 &pair_range,
					 int initial_writeback,
					 rtx *writeback_effect,
					 rtl_ssa::def_info **add_def,
					 rtl_ssa::def_info *base_def,
					 poly_int64 initial_offset,
					 unsigned access_size);
  int get_viable_bases (rtl_ssa::insn_info *insns[2],
			vec<base_cand> &base_cands,
			rtx cand_mems[2],
			unsigned access_size,
			bool reversed);
  void do_alias_analysis (rtl_ssa::insn_info *alias_hazards[4],
			  alias_walker *walkers[4],
			  bool load_p);
  void try_promote_writeback (rtl_ssa::insn_info *insn, bool load_p);
  void run ();
  ~pair_fusion ();
};
