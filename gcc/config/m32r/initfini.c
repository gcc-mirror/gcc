/* .init/.fini section handling + C++ global constructor/destructor handling.
   This file is based on crtstuff.c, sol2-crti.asm, sol2-crtn.asm.

Copyright (C) 1996, 1997, 1998 Free Software Foundation, Inc.

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

/* As a special exception, if you link this file with files
   compiled with GCC to produce an executable, this does not cause
   the resulting executable to be covered by the GNU General Public License.
   This exception does not however invalidate any other reasons why
   the executable file might be covered by the GNU General Public License.  */

/*  Declare a pointer to void function type.  */
typedef void (*func_ptr) (void);

#ifdef CRT_INIT

/* NOTE:  In order to be able to support SVR4 shared libraries, we arrange
   to have one set of symbols { __CTOR_LIST__, __DTOR_LIST__, __CTOR_END__,
   __DTOR_END__ } per root executable and also one set of these symbols
   per shared library.  So in any given whole process image, we may have
   multiple definitions of each of these symbols.  In order to prevent
   these definitions from conflicting with one another, and in order to
   ensure that the proper lists are used for the initialization/finalization
   of each individual shared library (respectively), we give these symbols
   only internal (i.e. `static') linkage, and we also make it a point to
   refer to only the __CTOR_END__ symbol in crtfini.o and the __DTOR_LIST__
   symbol in crtinit.o, where they are defined.  */

static func_ptr __CTOR_LIST__[1]
  __attribute__ ((section (".ctors")))
     = { (func_ptr) (-1) };

static func_ptr __DTOR_LIST__[1]
  __attribute__ ((section (".dtors")))
     = { (func_ptr) (-1) };

/* Run all the global destructors on exit from the program.  */
 
/* Some systems place the number of pointers in the first word of the
   table.  On SVR4 however, that word is -1.  In all cases, the table is
   null-terminated.  On SVR4, we start from the beginning of the list and
   invoke each per-compilation-unit destructor routine in order
   until we find that null.

   Note that this function MUST be static.  There will be one of these
   functions in each root executable and one in each shared library, but
   although they all have the same code, each one is unique in that it
   refers to one particular associated `__DTOR_LIST__' which belongs to the
   same particular root executable or shared library file.  */

static void __do_global_dtors ()
asm ("__do_global_dtors") __attribute__ ((section (".text")));

static void
__do_global_dtors ()
{
  func_ptr *p;

  for (p = __DTOR_LIST__ + 1; *p; p++)
    (*p) ();
}

/* .init section start.
   This must appear at the start of the .init section.  */

asm ("\n\
	.section .init,\"ax\",@progbits\n\
	.balign 4\n\
	.global __init\n\
__init:\n\
	push fp\n\
	push lr\n\
	mv fp,sp\n\
	ld24 r0,#__fini\n\
	bl atexit\n\
	.fillinsn\n\
");

/* .fini section start.
   This must appear at the start of the .init section.  */

asm ("\n\
	.section .fini,\"ax\",@progbits\n\
	.balign 4\n\
	.global __fini\n\
__fini:\n\
	push fp\n\
	push lr\n\
	mv fp,sp\n\
	bl __do_global_dtors\n\
	.fillinsn\n\
");

#endif /* CRT_INIT */

#ifdef CRT_FINI

/* Put a word containing zero at the end of each of our two lists of function
   addresses.  Note that the words defined here go into the .ctors and .dtors
   sections of the crtend.o file, and since that file is always linked in
   last, these words naturally end up at the very ends of the two lists
   contained in these two sections.  */

static func_ptr __CTOR_END__[1]
  __attribute__ ((section (".ctors")))
     = { (func_ptr) 0 };

static func_ptr __DTOR_END__[1]
  __attribute__ ((section (".dtors")))
     = { (func_ptr) 0 };

/* Run all global constructors for the program.
   Note that they are run in reverse order.  */

static void __do_global_ctors ()
asm ("__do_global_ctors") __attribute__ ((section (".text")));

static void
__do_global_ctors ()
{
  func_ptr *p;

  for (p = __CTOR_END__ - 1; *p != (func_ptr) -1; p--)
    (*p) ();
}

/* .init section end.
   This must live at the end of the .init section.  */

asm ("\n\
	.section .init,\"ax\",@progbits\n\
	bl __do_global_ctors\n\
	mv sp,fp\n\
	pop lr\n\
	pop fp\n\
	jmp lr\n\
	.fillinsn\n\
");

/* .fini section end.
   This must live at the end of the .fini section.  */

asm ("\n\
	.section .fini,\"ax\",@progbits\n\
	mv sp,fp\n\
	pop lr\n\
	pop fp\n\
	jmp lr\n\
	.fillinsn\n\
");

#endif /* CRT_FINI */
