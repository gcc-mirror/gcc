/* Definitions for PA_RISC with ELF-32 format
   Copyright (C) 2000, 2002 Free Software Foundation, Inc.

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

/* Turn off various SOM crap we don't want.  */
#undef TARGET_ELF32
#define TARGET_ELF32 1

/* Sibcalls are ok when ld is used in single subspace mode.  The
   multiple subspace mode is not compatible with sibcalls to external
   functions because the linker generated stubs store the return
   pointer into the frame.  This target does not need multiple
   subspace stubs, so we allow sibcalls to all functions.  */
#undef FUNCTION_OK_FOR_SIBCALL
#define FUNCTION_OK_FOR_SIBCALL(DECL) (!TARGET_PORTABLE_RUNTIME)

/* The libcall __canonicalize_funcptr_for_compare is referenced in
   crtend.o and the reference isn't resolved in objects that don't
   compare function pointers.  Thus, we need to play games to provide
   a reference in crtbegin.o.  The rest of the define is the same
   as that in crtstuff.c  */
#define CTOR_LIST_BEGIN \
  asm (".type __canonicalize_funcptr_for_compare,@function\n"		\
"	.text\n"							\
"	.word __canonicalize_funcptr_for_compare-$PIC_pcrel$0");	\
  STATIC func_ptr __CTOR_LIST__[1]					\
    __attribute__ ((__unused__, section(".ctors"),			\
		    aligned(sizeof(func_ptr))))				\
    = { (func_ptr) (-1) }
