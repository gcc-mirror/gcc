/* Prototypes of target machine for GNU compiler.  MIPS version.
   Copyright (C) 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998,
   1999, 2001, 2002, 2003, 2004 Free Software Foundation, Inc.
   Contributed by A. Lichnewsky (lich@inria.inria.fr).
   Changed by Michael Meissner	(meissner@osf.org).
   64 bit r4000 support by Ian Lance Taylor (ian@cygnus.com) and
   Brendan Eich (brendan@microunity.com).

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#ifndef GCC_MIPS_PROTOS_H
#define GCC_MIPS_PROTOS_H

extern int mips_regno_mode_ok_for_base_p (int, enum machine_mode, int);
extern int mips_address_insns (rtx, enum machine_mode);
extern int mips_const_insns (rtx);
extern int mips_fetch_insns (rtx);
extern bool mips_legitimate_address_p (enum machine_mode, rtx, int);
extern bool mips_legitimize_address (rtx *, enum machine_mode);
extern rtx mips_gotoff_global (rtx);
extern rtx mips_load_got_page (rtx);
extern rtx mips_load_got_global (rtx, rtx);
extern bool mips_legitimize_move (enum machine_mode, rtx, rtx);

extern int m16_uimm3_b (rtx, enum machine_mode);
extern int m16_simm4_1 (rtx, enum machine_mode);
extern int m16_nsimm4_1 (rtx, enum machine_mode);
extern int m16_simm5_1 (rtx, enum machine_mode);
extern int m16_nsimm5_1 (rtx, enum machine_mode);
extern int m16_uimm5_4 (rtx, enum machine_mode);
extern int m16_nuimm5_4 (rtx, enum machine_mode);
extern int m16_simm8_1 (rtx, enum machine_mode);
extern int m16_nsimm8_1 (rtx, enum machine_mode);
extern int m16_uimm8_1 (rtx, enum machine_mode);
extern int m16_nuimm8_1 (rtx, enum machine_mode);
extern int m16_uimm8_m1_1 (rtx, enum machine_mode);
extern int m16_uimm8_4 (rtx, enum machine_mode);
extern int m16_nuimm8_4 (rtx, enum machine_mode);
extern int m16_simm8_8 (rtx, enum machine_mode);
extern int m16_nsimm8_8 (rtx, enum machine_mode);
extern int m16_usym8_4 (rtx, enum machine_mode);
extern int m16_usym5_4 (rtx, enum machine_mode);

extern struct rtx_def *embedded_pic_fnaddr_reg (void);
extern struct rtx_def *embedded_pic_offset (rtx);
extern rtx mips_subword (rtx, int);
extern bool mips_split_64bit_move_p (rtx, rtx);
extern void mips_split_64bit_move (rtx, rtx);
extern const char *mips_output_move (rtx, rtx);
extern void mips_restore_gp (void);
#ifdef RTX_CODE
extern rtx gen_int_relational (enum rtx_code, rtx, rtx, rtx, int *);
extern void gen_conditional_branch (rtx *, enum rtx_code);
#endif
extern void gen_conditional_move (rtx *);
extern void mips_gen_conditional_trap (rtx *);
extern void mips_expand_call (rtx, rtx, rtx, rtx, int);
extern void mips_emit_fcc_reload (rtx, rtx, rtx);
extern void mips_set_return_address (rtx, rtx);
extern bool mips_expand_block_move (rtx, rtx, rtx);

extern void init_cumulative_args (CUMULATIVE_ARGS *, tree, rtx);
extern void function_arg_advance (CUMULATIVE_ARGS *, enum machine_mode,
				  tree, int);
extern struct rtx_def *function_arg (const CUMULATIVE_ARGS *,
				     enum machine_mode, tree, int);
extern int function_arg_partial_nregs (const CUMULATIVE_ARGS *,
				       enum machine_mode, tree, int);
extern bool mips_pad_arg_upward (enum machine_mode, tree);
extern bool mips_pad_reg_upward (enum machine_mode, tree);
extern int mips_setup_incoming_varargs (const CUMULATIVE_ARGS *,
					enum machine_mode, tree, int);
extern void mips_va_start (tree, rtx);
extern struct rtx_def *mips_va_arg (tree, tree);

extern bool mips_expand_unaligned_load (rtx, rtx, unsigned int, int);
extern bool mips_expand_unaligned_store (rtx, rtx, unsigned int, int);
extern void override_options (void);
extern void mips_conditional_register_usage (void);
extern void mips_order_regs_for_local_alloc (void);
extern HOST_WIDE_INT mips_debugger_offset (rtx, HOST_WIDE_INT);

extern void print_operand (FILE *, rtx, int);
extern void print_operand_address (FILE *, rtx);
extern int mips_output_external (FILE *, tree, const char *);
#if TARGET_IRIX
extern void irix_output_external_libcall (rtx);
#endif
extern void mips_output_filename (FILE *, const char *);
extern void mips_output_lineno (FILE *, int);
extern void mips_output_ascii (FILE *, const char *, size_t, const char *);
extern void mips_output_aligned_bss (FILE *, tree, const char *,
				     unsigned HOST_WIDE_INT, int);
extern void mips_output_aligned_decl_common (FILE *, tree, const char *,
					     unsigned HOST_WIDE_INT,
					     unsigned int);
extern void mips_declare_common_object (FILE *, const char *,
					const char *, unsigned HOST_WIDE_INT,
					unsigned int, bool);
extern void mips_declare_object (FILE *, const char *, const char *,
				 const char *, ...);
extern void mips_declare_object_name (FILE *, const char *, tree);
extern void mips_finish_declare_object (FILE *, tree, int, int);

extern rtx mips_rewrite_small_data (rtx);
extern HOST_WIDE_INT compute_frame_size (HOST_WIDE_INT);
extern HOST_WIDE_INT mips_initial_elimination_offset (int, int);
extern rtx mips_return_addr (int, rtx);
extern void mips_expand_prologue (void);
extern void mips_expand_epilogue (int);
extern int mips_can_use_return_insn (void);
extern struct rtx_def *mips_function_value (tree, tree, enum machine_mode);
extern int function_arg_pass_by_reference (const CUMULATIVE_ARGS *,
					   enum machine_mode, tree, int);

extern bool mips_cannot_change_mode_class (enum machine_mode,
					   enum machine_mode, enum reg_class);
extern bool mips_dangerous_for_la25_p (rtx);
extern enum reg_class mips_preferred_reload_class (rtx, enum reg_class);
extern enum reg_class mips_secondary_reload_class (enum reg_class,
						   enum machine_mode,
						   rtx, int);
extern int mips_class_max_nregs (enum reg_class, enum machine_mode);
extern bool mips_valid_pointer_mode (enum machine_mode);
extern int build_mips16_call_stub (rtx, rtx, rtx, int);
extern int mips_register_move_cost (enum machine_mode, enum reg_class,
				    enum reg_class);

extern int mips_adjust_insn_length (rtx, int);
extern const char *mips_output_load_label (void);
extern const char *mips_output_conditional_branch (rtx, rtx *, int, int,
						   int, int);
extern const char *mips_output_division (const char *, rtx *);
extern unsigned int mips_hard_regno_nregs (int, enum machine_mode);
extern int mips_return_in_memory (tree);
extern const char *mips_emit_prefetch (rtx *);

extern void irix_asm_output_align (FILE *, unsigned);
extern const char *current_section_name (void);
extern unsigned int current_section_flags (void);

#endif /* ! GCC_MIPS_PROTOS_H */
