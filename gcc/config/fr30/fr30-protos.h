/* Prototypes for fr30.c functions used in the md file & elsewhere.
   Copyright (C) 1999, 2000, 2002, 2004, 2007, 2010, 2011
   Free Software Foundation, Inc.

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

extern void  fr30_expand_prologue (void);
extern void  fr30_expand_epilogue (void);
extern unsigned int fr30_compute_frame_size (int, int);

#ifdef RTX_CODE
extern int   fr30_check_multiple_regs (rtx *, int, int);
extern void  fr30_print_operand (FILE *, rtx, int);
extern void  fr30_print_operand_address (FILE *, rtx);
extern rtx   fr30_move_double (rtx *);
#ifdef HAVE_MACHINE_MODES
extern int   fr30_const_double_is_zero (rtx);
#endif /* HAVE_MACHINE_MODES */
#endif /* RTX_CODE */
