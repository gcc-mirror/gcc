/* Specialized bits of code needed to support construction and
   destruction of file-scope objects in C++ code.

   Written by Ron Guilmette (rfg@ncd.com) with help from Richard Stallman.

Copyright (C) 1991 Free Software Foundation, Inc.

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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* As a special exception, if you link this library with files
   compiled with GCC to produce an executable, this does not cause
   the resulting executable to be covered by the GNU General Public License.
   This exception does not however invalidate any other reasons why
   the executable file might be covered by the GNU General Public License.  */

/* This file is a bit like libgcc1.c/libgcc2.c in that it is compiled
   multiple times and yields multiple .o files.

   This file is useful on target machines where the object file format
   supports multiple "user-defined" sections (e.g. COFF, ELF, ROSE).  On
   such systems, this file allows us to avoid running collect (or any
   other such slow and painful kludge).  Additionally, if the target
   system supports a .init section, this file allows us to support the
   linking of C++ code with a non-C++ main program.

   Note that if INIT_SECTION_ASM_OP is defined in the tm.h file, then
   this file *will* make use of the .init section.  If that symbol is
   not defined however, then the .init section will not be used.

   Currently, only ELF and COFF are supported.  It is likely however that
   ROSE could also be supported, if someone was willing to do the work to
   make whatever (small?) adaptations are needed.  (Some work may be
   needed on the ROSE assembler and linker also.)

   This file must be compiled with gcc.  */

/* It is incorrect to include config.h here, because this file is being
   compiled for the target, and hence definitions concerning only the host
   do not apply.  */

#include "tm.h"

#ifndef CTORS_SECTION_ASM_OP
#define CTORS_SECTION_ASM_OP	".section\t.ctors,\"a\",@progbits"
#endif
#ifndef DTORS_SECTION_ASM_OP
#define DTORS_SECTION_ASM_OP	".section\t.dtors,\"a\",@progbits"
#endif

#include "gbl-ctors.h"

#ifndef ON_EXIT
#define ON_EXIT(a, b)
#endif

#ifdef CRT_BEGIN

#ifdef INIT_SECTION_ASM_OP

/* The function __do_global_ctors_aux is compiled twice (once in crtbegin.o
   and once in crtend.o).  It must be declared static to avoid a link
   error.  Here, we define __do_global_ctors as an externally callable
   function.  It is externally callable so that __main can invoke it when
   INVOKE__main is defined.  This has the additional effect of forcing cc1
   to switch to the .text section.  */
static void __do_global_ctors_aux ();
void __do_global_ctors ()
{
#ifdef INVOKE__main  /* If __main won't actually call __do_global_ctors
			then it doesn't matter what's inside the function.
			The inside of __do_global_ctors_aux is called
			automatically in that case.
			And the Alliant fx2800 linker crashes
			on this reference.  So prevent the crash.  */
  __do_global_ctors_aux ();
#endif
}

asm (INIT_SECTION_ASM_OP);	/* cc1 doesn't know that we are switching! */

/* On some svr4 systems, the .init section preamble code provided in
   crti.o may do some evil things which we have to undo before we reach
   the function prologue code for __do_global_ctors (directly below).
   For such systems, define the macro INIT_SECTION_PREAMBLE to
   expand into the code needed to undo the actions of the crti.o file.  */
   
#ifdef INIT_SECTION_PREAMBLE
  INIT_SECTION_PREAMBLE;
#endif

/* A routine to invoke all of the global constructors upon entry to the
   program.  We put this into the .init section (for systems that have
   such a thing) so that we can properly perform the construction of
   file-scope static-storage C++ objects within shared libraries.   */

static void
__do_global_ctors_aux ()	/* prologue goes in .init section */
{
  asm (TEXT_SECTION_ASM_OP);	/* don't put epilogue and body in .init */
  DO_GLOBAL_CTORS_BODY;
  ON_EXIT (__do_global_dtors, 0);
}

#endif /* defined(INIT_SECTION_ASM_OP) */

/* Force cc1 to switch to .data section.  */
static func_ptr force_to_data[0] = { };

/* The -1 is a flag to __do_global_[cd]tors
   indicating that this table does not start with a count of elements.  */
#ifdef CTOR_LIST_BEGIN
CTOR_LIST_BEGIN;
#else
asm (CTORS_SECTION_ASM_OP);	/* cc1 doesn't know that we are switching! */
func_ptr __CTOR_LIST__[1] = { (func_ptr) (-1) };
#endif

#ifdef DTOR_LIST_BEGIN
DTOR_LIST_BEGIN;
#else
asm (DTORS_SECTION_ASM_OP);	/* cc1 doesn't know that we are switching! */
func_ptr __DTOR_LIST__[1] = { (func_ptr) (-1) };
#endif

#endif /* defined(CRT_BEGIN) */

#ifdef CRT_END

#ifdef INIT_SECTION_ASM_OP

/* A routine to invoke all of the global constructors upon entry to the
   program.  We put this into the .init section (for systems that have
   such a thing) so that we can properly perform the construction of
   file-scope static-storage C++ objects within shared libraries.

   This must be virtually identical to the one above so that we can
   insure that the function prologue from the one above works correctly
   with the epilogue from this one.  (They will both go into the .init
   section as the first and last things (respectively) that the linker
   will put in that section.)
*/

static void
__do_global_ctors_aux ()	/* prologue goes in .text section */
{
  asm (INIT_SECTION_ASM_OP);
  DO_GLOBAL_CTORS_BODY;
  ON_EXIT (__do_global_dtors, 0);
}				/* epilogue and body go in .init section */

#endif /* defined(INIT_SECTION_ASM_OP) */

/* Force cc1 to switch to .data section.  */
static func_ptr force_to_data[0] = { };

#ifdef CTOR_LIST_END
CTOR_LIST_END;
#else
asm (CTORS_SECTION_ASM_OP);	/* cc1 doesn't know that we are switching! */
func_ptr __CTOR_END__[1] = { (func_ptr) 0 };
#endif

#ifdef DTOR_LIST_END
DTOR_LIST_END;
#else
asm (DTORS_SECTION_ASM_OP);	/* cc1 doesn't know that we are switching! */
func_ptr __DTOR_END__[1] = { (func_ptr) 0 };
#endif

#endif /* defined(CRT_END) */
