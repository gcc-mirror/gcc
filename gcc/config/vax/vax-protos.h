/* Definitions of target machine for GNU compiler.  VAX version.
   Copyright (C) 2000 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#ifdef RTX_CODE
extern const char *rev_cond_name PARAMS ((rtx));
extern void split_quadword_operands PARAMS ((rtx *, rtx *, int));
extern void print_operand_address PARAMS ((FILE *, rtx));
extern int vax_float_literal PARAMS ((rtx));
extern int vax_address_cost PARAMS ((rtx));
extern int vax_rtx_cost PARAMS ((rtx));
extern int reg_was_0_p PARAMS ((rtx, rtx));
#endif /* RTX_CODE */

#ifdef REAL_VALUE_TYPE
extern int check_float_value PARAMS ((enum machine_mode, REAL_VALUE_TYPE *, int));
#endif /* REAL_VALUE_TYPE */

#ifdef TREE_CODE
extern void vms_check_external PARAMS ((tree, const char *, int));
#endif /* TREE_CODE */

extern void vms_flush_pending_externals PARAMS ((FILE *));
extern void const_section PARAMS ((void));
