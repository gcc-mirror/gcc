/* IA32 VxWorks and VxWorks AE target definitions.
   Copyright (C) 2007, 2008, 2010 Free Software Foundation, Inc.

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

/* VxWorks uses the same ABI as Solaris 2.  */

#define SUBTARGET_RETURN_IN_MEMORY(TYPE, FNTYPE) \
	ix86_solaris_return_in_memory (TYPE, FNTYPE)

/* Provide our target specific DBX_REGISTER_NUMBER, as advertised by the
   common svr4.h.  VxWorks relies on the SVR4 numbering.  */

#undef DBX_REGISTER_NUMBER
#define DBX_REGISTER_NUMBER(n)  svr4_dbx_register_map[n]
