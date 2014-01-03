/* Definitions of target machine for GNU compiler.  VAX version.
   Copyright (C) 2000-2014 Free Software Foundation, Inc.

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

extern bool legitimate_constant_address_p (rtx);
extern void vax_expand_prologue (void);

#ifdef RTX_CODE
extern const char *cond_name (rtx);
extern bool adjacent_operands_p (rtx, rtx, enum machine_mode);
extern const char *rev_cond_name (rtx);
extern void print_operand_address (FILE *, rtx);
extern void print_operand (FILE *, rtx, int);
extern void vax_notice_update_cc (rtx, rtx);
extern void vax_expand_addsub_di_operands (rtx *, enum rtx_code);
extern const char * vax_output_int_move (rtx, rtx *, enum machine_mode);
extern const char * vax_output_int_add (rtx, rtx *, enum machine_mode);
extern const char * vax_output_int_subtract (rtx, rtx *, enum machine_mode);
extern const char * vax_output_movmemsi (rtx, rtx *);
#endif /* RTX_CODE */

#ifdef REAL_VALUE_TYPE
extern int check_float_value (enum machine_mode, REAL_VALUE_TYPE *, int);
#endif /* REAL_VALUE_TYPE */
