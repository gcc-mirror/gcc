/* Definitions of target machine for GNU compiler.  VAX version.
   Copyright (C) 2000, 2002, 2003, 2004 Free Software Foundation, Inc.

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

extern void override_options (void);

#ifdef RTX_CODE
extern const char *rev_cond_name (rtx);
extern void split_quadword_operands (rtx *, rtx *, int);
extern void print_operand_address (FILE *, rtx);
extern int vax_float_literal (rtx);
extern void vax_notice_update_cc (rtx, rtx);
#endif /* RTX_CODE */

#ifdef REAL_VALUE_TYPE
extern int check_float_value (enum machine_mode, REAL_VALUE_TYPE *, int);
#endif /* REAL_VALUE_TYPE */

