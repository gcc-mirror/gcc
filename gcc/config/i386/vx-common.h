/* IA32 VxWorks and VxWorks AE target definitions.
   Copyright (C) 2007-2013 Free Software Foundation, Inc.

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

#define ASM_OUTPUT_ALIGNED_BSS(FILE, DECL, NAME, SIZE, ALIGN) \
  asm_output_aligned_bss (FILE, DECL, NAME, SIZE, ALIGN)

/* VxWorks uses the same ABI as Solaris 2, so use i386/sol2.h version.  */

#undef TARGET_SUBTARGET_DEFAULT
#define TARGET_SUBTARGET_DEFAULT \
	(MASK_80387 | MASK_IEEE_FP | MASK_FLOAT_RETURNS | MASK_VECT8_RETURNS)

/* Provide our target specific DBX_REGISTER_NUMBER.  VxWorks relies on
   the SVR4 numbering.  */

#undef DBX_REGISTER_NUMBER
#define DBX_REGISTER_NUMBER(n)  svr4_dbx_register_map[n]
