/* Specialized code needed to support construction and destruction of
   file-scope objects in C++ and Java code, and to support exception handling.
   Copyright (C) 2005 Free Software Foundation, Inc.
   Contributed by Analog Devices.

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
the Free Software Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

/* As a special exception, if you link this library with files
   compiled with GCC to produce an executable, this does not cause
   the resulting executable to be covered by the GNU General Public License.
   This exception does not however invalidate any other reasons why
   the executable file might be covered by the GNU General Public License.  */

/*
 * This file just supplies function prologues for the .init and .fini
 * sections.  It is linked in before crtbegin.o.
 */

	.file   "crti.o"
	.ident  "GNU C crti.o"

	.section .init
	.globl  __init
	.type   __init,@function
__init:
#if defined __ID_SHARED_LIB__
	[--SP] = P5;
#elif defined __BFIN_FDPIC__
	[--SP] = P3; 
#endif
	LINK 12;
#if defined __ID_SHARED_LIB__
	P5 = [P5 + _current_shared_library_p5_offset_]
#endif	
	.section .fini
	.globl  __fini
	.type   __fini,@function
__fini:
#if defined __ID_SHARED_LIB__
	[--SP] = P5; 
#elif defined __BFIN_FDPIC__
	[--SP] = P3; 
#endif
	LINK 12; 
#if defined __ID_SHARED_LIB__
	P5 = [P5 + _current_shared_library_p5_offset_]
#endif	
