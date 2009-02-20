/* score7.h for Sunplus S+CORE processor
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

#ifndef GCC_SCORE7_H
#define GCC_SCORE7_H

enum score7_address_type
{
  SCORE7_ADD_REG,
  SCORE7_ADD_CONST_INT,
  SCORE7_ADD_SYMBOLIC
};

struct score7_frame_info
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

struct score7_arg_info
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
struct score7_address_info
{
  enum score7_address_type type;
  rtx reg;
  rtx offset;
  enum rtx_code code;
  enum score_symbol_type symbol_type;
};
#endif

#define SCORE7_SDATA_MAX                score7_sdata_max
#define SCORE7_STACK_ALIGN(LOC)         (((LOC) + 3) & ~3)
#define SCORE7_PROLOGUE_TEMP_REGNUM     (GP_REG_FIRST + 8)
#define SCORE7_EPILOGUE_TEMP_REGNUM     (GP_REG_FIRST + 8)
#define SCORE7_DEFAULT_SDATA_MAX        8

extern int score7_symbolic_constant_p (rtx x,
                                       enum score_symbol_type *symbol_type);
extern bool score7_return_in_memory (tree type,
                                     tree fndecl ATTRIBUTE_UNUSED);
extern void score7_output_mi_thunk (FILE *file,
                                    tree thunk_fndecl ATTRIBUTE_UNUSED,
                                    HOST_WIDE_INT delta,
                                    HOST_WIDE_INT vcall_offset,
                                    tree function);
extern int score7_legitimize_address (rtx *xloc);
extern void
score7_function_prologue (FILE *file,
                          HOST_WIDE_INT size ATTRIBUTE_UNUSED);
extern void
score7_function_epilogue (FILE *file,
                          HOST_WIDE_INT size ATTRIBUTE_UNUSED);
extern section *score7_select_rtx_section (enum machine_mode mode, rtx x,
                                           unsigned HOST_WIDE_INT align);
extern bool score7_in_small_data_p (tree decl);
extern void score7_asm_file_start (void);
extern void score7_asm_file_end (void);
extern void score7_override_options (void);
extern int score7_reg_class (int regno);
extern enum reg_class score7_preferred_reload_class (rtx x ATTRIBUTE_UNUSED,
                                                     enum reg_class rclass);
extern enum
reg_class score7_secondary_reload_class (enum reg_class rclass,
                                         enum machine_mode mode ATTRIBUTE_UNUSED,
                                         rtx x);
extern int score7_const_ok_for_letter_p (HOST_WIDE_INT value, char c);
extern int score7_extra_constraint (rtx op, char c);
extern int score7_hard_regno_mode_ok (unsigned int regno,
                                      enum machine_mode mode);
extern HOST_WIDE_INT
score7_initial_elimination_offset (int from,
                                   int to ATTRIBUTE_UNUSED);
extern void score7_function_arg_advance (CUMULATIVE_ARGS *cum,
                                         enum machine_mode mode,
                                         tree type,
                                         int named);
extern int score7_arg_partial_bytes (CUMULATIVE_ARGS *cum,
                                     enum machine_mode mode,
                                     tree type,
                                     bool named);
extern rtx score7_function_arg (const CUMULATIVE_ARGS *cum,
                                enum machine_mode mode,
                                tree type,
                                int named);
extern rtx score7_function_value (tree valtype,
                                  tree func ATTRIBUTE_UNUSED,
                                  enum machine_mode mode);
extern void score7_initialize_trampoline (rtx ADDR, rtx FUNC, rtx CHAIN);
extern int score7_regno_mode_ok_for_base_p (int regno, int strict);
extern int score7_address_p (enum machine_mode mode, rtx x, int strict);
extern int score7_register_move_cost (enum machine_mode mode ATTRIBUTE_UNUSED,
                                      enum reg_class from,
                                      enum reg_class to);
extern bool score7_rtx_costs (rtx x, int code, int outer_code, int *total, bool speed);
extern int score7_address_cost (rtx addr);
extern int score7_output_external (FILE *file ATTRIBUTE_UNUSED,
                                   tree decl,
                                   const char *name);
extern rtx score7_return_addr (int count, rtx frame ATTRIBUTE_UNUSED);
extern void score7_print_operand (FILE *file, rtx op, int c);
extern void score7_print_operand_address (FILE *file, rtx x);
extern enum machine_mode score7_select_cc_mode (enum rtx_code op,
                                                rtx x,
                                                rtx y);
extern void score7_prologue (void);
extern void score7_epilogue (int sibcall_p);
extern void score7_gen_cmp (enum machine_mode mode);
extern void score7_call (rtx *ops, bool sib);
extern void score7_call_value (rtx *ops, bool sib);
extern void score7_movsicc (rtx *ops);
extern void score7_movdi (rtx *ops);
extern void score7_zero_extract_andi (rtx *ops);
extern const char * score7_select_add_imm (rtx *ops, bool set_cc);
extern const char * score7_select (rtx *ops, const char *inst_pre, bool commu,
                                   const char *letter, bool set_cc);
extern const char * score7_move (rtx *ops);
extern const char * score7_limm (rtx *ops);
extern const char *
score7_linsn (rtx *ops, enum score_mem_unit unit, bool sign);
extern const char *
score7_sinsn (rtx *ops, enum score_mem_unit unit);
#endif
