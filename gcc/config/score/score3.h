/* score3.h for Sunplus S+CORE processor
   Copyright (C) 2005, 2007, 2008 Free Software Foundation, Inc.
   Contributed by Sunnorth

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#ifndef GCC_SCORE3_H
#define GCC_SCORE3_H

enum score3_address_type
{
  SCORE3_ADD_REG,
  SCORE3_ADD_CONST_INT,
  SCORE3_ADD_SYMBOLIC
};

struct score3_frame_info
{
  HOST_WIDE_INT total_size;       /* bytes that the entire frame takes up  */
  HOST_WIDE_INT var_size;         /* bytes that variables take up  */
  HOST_WIDE_INT args_size;        /* bytes that outgoing arguments take up  */
  HOST_WIDE_INT gp_reg_size;      /* bytes needed to store gp regs  */
  HOST_WIDE_INT gp_sp_offset;     /* offset from new sp to store gp registers  */
  HOST_WIDE_INT cprestore_size;   /* # bytes that the .cprestore slot takes up  */
  unsigned int  mask;             /* mask of saved gp registers  */
  int num_gp;                     /* number of gp registers saved  */
};

struct score3_arg_info
{
  unsigned int num_bytes;     /* The argument's size in bytes  */
  unsigned int reg_words;     /* The number of words passed in registers  */
  unsigned int reg_offset;    /* The offset of the first register from  */
                              /* GP_ARG_FIRST or FP_ARG_FIRST etc  */
  unsigned int stack_words;   /* The number of words that must be passed  */
                              /* on the stack  */
  unsigned int stack_offset;  /* The offset from the start of the stack  */
                              /* overflow area  */
};

#ifdef RTX_CODE
struct score3_address_info
{
  enum score3_address_type type;
  rtx reg;
  rtx offset;
  enum rtx_code code;
  enum score_symbol_type symbol_type;
};
#endif

#define SCORE3_SDATA_MAX                score3_sdata_max
#define SCORE3_STACK_ALIGN(LOC)         (((LOC) + 3) & ~3)
#define SCORE3_PROLOGUE_TEMP_REGNUM     (GP_REG_FIRST + 8)
#define SCORE3_EPILOGUE_TEMP_REGNUM     (GP_REG_FIRST + 8)
#define SCORE3_DEFAULT_SDATA_MAX        8

extern int score3_symbolic_constant_p (rtx x,
                                       enum score_symbol_type *symbol_type);
extern bool score3_return_in_memory (tree type,
                                     tree fndecl ATTRIBUTE_UNUSED);
extern void score3_output_mi_thunk (FILE *file,
                                    tree thunk_fndecl ATTRIBUTE_UNUSED,
                                    HOST_WIDE_INT delta,
                                    HOST_WIDE_INT vcall_offset,
                                    tree function);
extern rtx score3_legitimize_address (rtx x);
extern void
score3_function_prologue (FILE *file,
                          HOST_WIDE_INT size ATTRIBUTE_UNUSED);
extern void
score3_function_epilogue (FILE *file,
                          HOST_WIDE_INT size ATTRIBUTE_UNUSED);
extern section *score3_select_rtx_section (enum machine_mode mode, rtx x,
                                           unsigned HOST_WIDE_INT align);
extern bool score3_in_small_data_p (tree decl);
extern void score3_asm_file_start (void);
extern void score3_asm_file_end (void);
extern void score3_override_options (void);
extern int score3_reg_class (int regno);
extern enum reg_class score3_preferred_reload_class (rtx x ATTRIBUTE_UNUSED,
                                                     enum reg_class rclass);
extern enum reg_class
score3_secondary_reload_class (enum reg_class rclass,
                               enum machine_mode mode ATTRIBUTE_UNUSED,
                               rtx x);
extern int score3_const_ok_for_letter_p (HOST_WIDE_INT value, char c);
extern int score3_extra_constraint (rtx op, char c);
extern int score3_hard_regno_mode_ok (unsigned int regno,
                                      enum machine_mode mode);
extern HOST_WIDE_INT
score3_initial_elimination_offset (int from,
                                   int to ATTRIBUTE_UNUSED);
extern void score3_function_arg_advance (CUMULATIVE_ARGS *cum,
                                         enum machine_mode mode,
                                         tree type,
                                         int named);
extern int score3_arg_partial_bytes (CUMULATIVE_ARGS *cum,
                                     enum machine_mode mode,
                                     tree type,
                                     bool named);
extern rtx score3_function_arg (const CUMULATIVE_ARGS *cum,
                                enum machine_mode mode,
                                tree type,
                                int named);
extern rtx score3_function_value (tree valtype,
                                  tree func ATTRIBUTE_UNUSED,
                                  enum machine_mode mode);
extern void score3_initialize_trampoline (rtx ADDR, rtx FUNC, rtx CHAIN);
extern int score3_regno_mode_ok_for_base_p (int regno, int strict);
extern bool score3_legitimate_address_p (enum machine_mode mode, rtx x,
					 bool strict);
extern int score3_register_move_cost (enum machine_mode mode ATTRIBUTE_UNUSED,
                                      enum reg_class from,
                                      enum reg_class to);
extern bool score3_rtx_costs (rtx x, int code, int outer_code, int *total, bool speed);
extern int score3_address_cost (rtx addr);
extern int score3_output_external (FILE *file ATTRIBUTE_UNUSED,
                                   tree decl,
                                   const char *name);
extern rtx score3_return_addr (int count, rtx frame ATTRIBUTE_UNUSED);
extern void score3_print_operand (FILE *file, rtx op, int c);
extern void score3_print_operand_address (FILE *file, rtx x);
extern enum machine_mode
score3_select_cc_mode (enum rtx_code op, rtx x, rtx y);
extern void score3_prologue (void);
extern void score3_epilogue (int sibcall_p);
extern void score3_call (rtx *ops, bool sib);
extern void score3_call_value (rtx *ops, bool sib);
extern void score3_movsicc (rtx *ops);
extern void score3_movdi (rtx *ops);
extern void score3_zero_extract_andi (rtx *ops);
extern const char * score3_select_add_imm (rtx *ops, bool set_cc);
extern const char * score3_select (rtx *ops, const char *inst_pre, bool commu,
                                   const char *letter, bool set_cc);
extern const char * score3_move (rtx *ops);
extern const char * score3_limm (rtx *ops);
extern const char *
score3_linsn (rtx *ops, enum score_mem_unit unit, bool sign);
extern const char *
score3_sinsn (rtx *ops, enum score_mem_unit unit);
extern const char * score3_output_casesi (rtx *operands);
extern const char * score3_rpush (rtx *ops);
extern const char * score3_rpop (rtx *ops);
#endif
