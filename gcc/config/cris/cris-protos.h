/* Definitions for GCC.  Part of the machine description for CRIS.
   Copyright (C) 1998, 1999, 2000, 2001 Free Software Foundation, Inc.
   Contributed by Axis Communications.

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

/* Prototypes for the CRIS port.  */

#if defined(FILE) || defined(stdin) || defined(stdout) || defined(getc) || defined(putc)
#define STDIO_INCLUDED
#endif

extern void cris_conditional_register_usage PARAMS ((void));
extern int cris_simple_epilogue PARAMS ((void));
#ifdef RTX_CODE
extern const char *cris_op_str PARAMS ((rtx));
extern int cris_eligible_for_epilogue_delay PARAMS ((rtx));
extern void cris_notice_update_cc PARAMS ((rtx, rtx));
extern int cris_address_cost PARAMS ((rtx));
extern void cris_print_operand PARAMS ((FILE *, rtx, int));
extern void cris_print_operand_address PARAMS ((FILE *, rtx));
extern int cris_side_effect_mode_ok PARAMS ((enum rtx_code, rtx *, int, int,
                                             int, int, int));
extern rtx cris_return_addr_rtx PARAMS ((int, rtx));
extern rtx cris_split_movdx PARAMS ((rtx *));
extern int cris_legitimate_pic_operand PARAMS ((rtx));
extern int cris_gotless_symbol PARAMS ((rtx));
extern int cris_got_symbol PARAMS ((rtx));
extern int cris_symbol PARAMS ((rtx));
extern void cris_output_addr_const PARAMS ((FILE *, rtx));
extern int cris_cfun_uses_pic_table PARAMS ((void));
extern void cris_target_asm_named_section
  PARAMS ((const char *, unsigned int));

# ifdef TREE_CODE
extern rtx cris_expand_builtin_va_arg PARAMS ((tree, tree));
# endif
#endif /* RTX_CODE */

#ifdef GCC_C_PRAGMA_H
extern void cris_pragma_expand_mul PARAMS ((cpp_reader *));
#endif

/* Need one that returns an int; usable in expressions. */
extern int cris_fatal PARAMS ((char *));

extern void cris_override_options PARAMS ((void));

extern int cris_initial_elimination_offset PARAMS ((int, int));

extern void cris_init_expanders PARAMS ((void));

extern int cris_delay_slots_for_epilogue PARAMS ((void));
