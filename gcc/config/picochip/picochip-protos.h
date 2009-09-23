/* Prototypes for exported functions defined in picochip.c

   Copyright (C) 2000, 2001, 2008 Free Software Foundation, Inc.
   Contributed by picoChip Designs Ltd. (http://www.picochip.com)
   Maintained by Daniel Towner (daniel.towner@picochip.com) and
   Hariharan Sandanagobalane (hariharan@picochip.com).

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
   along with GCC; see the file COPYING3.  If not, see
   <http://www.gnu.org/licenses/>. */

#include "target.h"
extern void picochip_function_prologue (FILE *, HOST_WIDE_INT);
extern void picochip_function_epilogue (FILE *, HOST_WIDE_INT);

extern enum reg_class picochip_reg_class_from_letter (unsigned);
extern int picochip_const_ok_for_letter_p (unsigned HOST_WIDE_INT value, unsigned c);

#ifdef RTX_CODE			/* inside TREE_CODE */

extern int picochip_reg_mode_ok_for_base_p (int mode, rtx x, unsigned strict);
extern void picochip_print_operand (FILE * file, rtx op, int letter);
extern void picochip_print_operand_address (FILE * file, rtx operand);

extern const char *picochip_output_cbranch (rtx operands[]);
extern const char *picochip_output_branch (rtx operands[], rtx insn);
extern const char *picochip_output_compare (rtx operands[]);
extern const char *picochip_output_jump (rtx insn);

extern const char *picochip_output_put_array (int alternative,
					      rtx operands[]);
extern const char *picochip_output_get_array (int alternative,
					      rtx operands[]);
extern const char *picochip_output_testport_array (int alternative,
						   rtx operands[]);

extern rtx gen_SImode_mem(rtx opnd1,rtx opnd2);
extern bool ok_to_peephole_stw(rtx opnd0, rtx opnd1, rtx opnd2, rtx opnd3);
extern bool ok_to_peephole_ldw(rtx opnd0, rtx opnd1, rtx opnd2, rtx opnd3);

extern rtx gen_min_reg(rtx opnd1,rtx opnd2);

extern rtx picochip_function_arg (CUMULATIVE_ARGS cum, int mode, tree type,
			   int named);

extern rtx picochip_incoming_function_arg (CUMULATIVE_ARGS, int, tree, int);
extern CUMULATIVE_ARGS picochip_arg_advance (CUMULATIVE_ARGS cum, int mode,
				      tree type, int named);

extern int picochip_regno_nregs (int regno, int mode);
extern int picochip_class_max_nregs (int klass, int mode);

extern void picochip_order_regs_for_local_alloc (void);

extern int picochip_word_aligned_memory_reference (rtx operand);
extern int picochip_alignable_memory_operand (rtx operand, enum machine_mode mode);
extern int picochip_absolute_memory_operand (rtx op, enum machine_mode mode);

extern rtx picochip_function_value (const_tree valtype, const_tree func, bool outgoing);
extern int picochip_symbol_offset (rtx operand);

extern int picochip_get_function_arg_boundary (enum machine_mode mode);

extern enum reg_class picochip_secondary_reload(bool in_p,
                                 rtx x,
                                 enum reg_class cla,
                                 enum machine_mode mode,
                                 secondary_reload_info *sri);

extern void picochip_get_hi_aligned_mem (rtx ref, rtx * paligned_mem, rtx * pbitnum);

extern rtx picochip_get_low_const (rtx value);
extern rtx picochip_get_high_const (rtx value);

extern void picochip_expand_prologue (void);
extern void picochip_expand_epilogue (int is_sibling_call);

extern void picochip_final_prescan_insn (rtx insn, rtx * operand, int num_operands);
extern const char *picochip_asm_output_opcode (FILE * f, const char *ptr);
extern void picochip_override_options (void);

extern int picochip_check_conditional_copy (rtx * operands);

extern rtx picochip_return_addr_rtx(int count, rtx frameaddr);
extern rtx picochip_struct_value_rtx(tree fntype ATTRIBUTE_UNUSED,
                              int incoming ATTRIBUTE_UNUSED);

#endif /* RTX_CODE inside TREE_CODE */

void picochip_output_ascii (FILE * file, const char *str, int length);

extern int picochip_hard_regno_mode_ok (int regno, enum machine_mode mode);
extern void picochip_generate_internal_label (char *str, const char *prefix,
					      long num);

extern bool picochip_return_in_memory(const_tree type,
                                      const_tree fntype ATTRIBUTE_UNUSED);

extern int initial_elimination_offset (int from, int to);

extern void picochip_output_aligned_common (FILE * stream, const char *name,
					    unsigned size, unsigned align);

extern void picochip_output_global (FILE * stream, const char *name);

extern void picochip_output_aligned_local (FILE * stream, const char *name,
					   unsigned size, unsigned alignment);

extern void picochip_output_label (FILE * stream, const char name[]);
extern void picochip_output_labelref (FILE * stream, const char name[]);
extern void picochip_weaken_label (FILE * stream, const char name[]);
extern void picochip_output_internal_label (FILE * stream, const char *prefix,
				     unsigned long num);

extern void warn_of_byte_access (void);

/* True if VLIW scheduling is enabled (i.e., second scheduling pass). */
extern int picochip_flag_schedule_insns2;

extern void picochip_asm_output_anchor (rtx symbol);

/* Instruction set capability flags.  These are initialised to the
   appropriate values by picochip_override_options, once the user has
   selected a CPU type. */
extern bool picochip_has_mul_unit;
extern bool picochip_has_mac_unit;

