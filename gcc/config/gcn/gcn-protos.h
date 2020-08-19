/* Copyright (C) 2016-2020 Free Software Foundation, Inc.

   This file is free software; you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free
   Software Foundation; either version 3 of the License, or (at your option)
   any later version.

   This file is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
   for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#ifndef _GCN_PROTOS_
#define _GCN_PROTOS_

extern void gcn_asm_output_symbol_ref (FILE *file, rtx x);
extern tree gcn_builtin_decl (unsigned code, bool initialize_p);
extern bool gcn_can_split_p (machine_mode, rtx);
extern bool gcn_constant64_p (rtx);
extern bool gcn_constant_p (rtx);
extern rtx gcn_convert_mask_mode (rtx reg);
extern unsigned int gcn_dwarf_register_number (unsigned int regno);
extern char * gcn_expand_dpp_shr_insn (machine_mode, const char *, int, int);
extern void gcn_expand_epilogue ();
extern rtx gcn_expand_scaled_offsets (addr_space_t as, rtx base, rtx offsets,
				      rtx scale, bool unsigned_p, rtx exec);
extern void gcn_expand_prologue ();
extern rtx gcn_expand_reduc_scalar (machine_mode, rtx, int);
extern rtx gcn_expand_scalar_to_vector_address (machine_mode, rtx, rtx, rtx);
extern void gcn_expand_vector_init (rtx, rtx);
extern bool gcn_flat_address_p (rtx, machine_mode);
extern bool gcn_fp_constant_p (rtx, bool);
extern rtx gcn_full_exec ();
extern rtx gcn_full_exec_reg ();
extern rtx gcn_gen_undef (machine_mode);
extern bool gcn_global_address_p (rtx);
extern tree gcn_goacc_adjust_propagation_record (tree record_type, bool sender,
						 const char *name);
extern void gcn_goacc_adjust_gangprivate_decl (tree var);
extern void gcn_goacc_reduction (gcall *call);
extern bool gcn_hard_regno_rename_ok (unsigned int from_reg,
				      unsigned int to_reg);
extern machine_mode gcn_hard_regno_caller_save_mode (unsigned int regno,
						     unsigned int nregs,
						     machine_mode regmode);
extern bool gcn_hard_regno_mode_ok (int regno, machine_mode mode);
extern int gcn_hard_regno_nregs (int regno, machine_mode mode);
extern void gcn_hsa_declare_function_name (FILE *file, const char *name,
					   tree decl);
extern HOST_WIDE_INT gcn_initial_elimination_offset (int, int);
extern bool gcn_inline_constant64_p (rtx, bool);
extern bool gcn_inline_constant_p (rtx);
extern int gcn_inline_fp_constant_p (rtx, bool);
extern reg_class gcn_mode_code_base_reg_class (machine_mode, addr_space_t,
					       int, int);
extern rtx gcn_oacc_dim_pos (int dim);
extern rtx gcn_oacc_dim_size (int dim);
extern rtx gcn_operand_doublepart (machine_mode, rtx, int);
extern rtx gcn_operand_part (machine_mode, rtx, int);
extern bool gcn_regno_mode_code_ok_for_base_p (int, machine_mode,
					       addr_space_t, int, int);
extern reg_class gcn_regno_reg_class (int regno);
extern rtx gcn_scalar_exec ();
extern rtx gcn_scalar_exec_reg ();
extern bool gcn_scalar_flat_address_p (rtx);
extern bool gcn_scalar_flat_mem_p (rtx);
extern bool gcn_sgpr_move_p (rtx, rtx);
extern bool gcn_valid_move_p (machine_mode, rtx, rtx);
extern rtx gcn_vec_constant (machine_mode, int);
extern rtx gcn_vec_constant (machine_mode, rtx);
extern bool gcn_vgpr_move_p (rtx, rtx);
extern void print_operand_address (FILE *file, register rtx addr);
extern void print_operand (FILE *file, rtx x, int code);
extern bool regno_ok_for_index_p (int);

enum gcn_cvt_t
{
  fix_trunc_cvt,
  fixuns_trunc_cvt,
  float_cvt,
  floatuns_cvt,
  extend_cvt,
  trunc_cvt
};

extern bool gcn_valid_cvt_p (machine_mode from, machine_mode to,
			     enum gcn_cvt_t op);

#ifdef TREE_CODE
extern void gcn_init_cumulative_args (CUMULATIVE_ARGS *, tree, rtx, tree,
				      int);
class gimple_opt_pass;
extern gimple_opt_pass *make_pass_omp_gcn (gcc::context *ctxt);
#endif

/* Return true if MODE is valid for 1 VGPR register.  */

inline bool
vgpr_1reg_mode_p (machine_mode mode)
{
  return (mode == SImode || mode == SFmode || mode == HImode || mode == QImode
	  || mode == V64QImode || mode == V64HImode || mode == V64SImode
	  || mode == V64HFmode || mode == V64SFmode || mode == BImode);
}

/* Return true if MODE is valid for 1 SGPR register.  */

inline bool
sgpr_1reg_mode_p (machine_mode mode)
{
  return (mode == SImode || mode == SFmode || mode == HImode
	  || mode == QImode || mode == BImode);
}

/* Return true if MODE is valid for pair of VGPR registers.  */

inline bool
vgpr_2reg_mode_p (machine_mode mode)
{
  return (mode == DImode || mode == DFmode
	  || mode == V64DImode || mode == V64DFmode);
}

/* Return true if MODE can be handled directly by VGPR operations.  */

inline bool
vgpr_vector_mode_p (machine_mode mode)
{
  return (mode == V64QImode || mode == V64HImode
	  || mode == V64SImode || mode == V64DImode
	  || mode == V64HFmode || mode == V64SFmode || mode == V64DFmode);
}


/* Return true if MODE is valid for pair of SGPR registers.  */

inline bool
sgpr_2reg_mode_p (machine_mode mode)
{
  return mode == DImode || mode == DFmode;
}

#endif
