/* Specialized bits of code needed to support construction and
   destruction of file-scope objects in C++ code.
   Copyright (C) 1991, 1994-1999, 2000 Free Software Foundation, Inc.
   Contributed by Ron Guilmette (rfg@monkeys.com).

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
#include "tsystem.h"

#include "defaults.h"
#include "frame.h"

/* We do not want to add the weak attribute to the declarations of these
   routines in frame.h because that will cause the definition of these
   symbols to be weak as well.

   This exposes a core issue, how to handle creating weak references vs
   how to create weak definitions.  Either we have to have the definition
   of TARGET_WEAK_ATTRIBUTE be conditional in the shared header files or
   have a second declaration if we want a function's references to be weak,
   but not its definition.

   Making TARGET_WEAK_ATTRIBUTE conditional seems like a good solution until
   one thinks about scaling to larger problems -- ie, the condition under
   which TARGET_WEAK_ATTRIBUTE is active will eventually get far too
   complicated.

   So, we take an approach similar to #pragma weak -- we have a second
   declaration for functions that we want to have weak references.

   Neither way is particularly good.  */
   
/* References to __register_frame_info and __deregister_frame_info should
   be weak in this file if at all possible.  */
extern void __register_frame_info (void *, struct object *)
				  TARGET_ATTRIBUTE_WEAK;

extern void *__deregister_frame_info (void *)
				     TARGET_ATTRIBUTE_WEAK;

#ifndef OBJECT_FORMAT_MACHO

/* Provide default definitions for the pseudo-ops used to switch to the
   .ctors and .dtors sections.
 
   Note that we want to give these sections the SHF_WRITE attribute
   because these sections will actually contain data (i.e. tables of
   addresses of functions in the current root executable or shared library
   file) and, in the case of a shared library, the relocatable addresses
   will have to be properly resolved/relocated (and then written into) by
   the dynamic linker when it actually attaches the given shared library
   to the executing process.  (Note that on SVR4, you may wish to use the
   `-z text' option to the ELF linker, when building a shared library, as
   an additional check that you are doing everything right.  But if you do
   use the `-z text' option when building a shared library, you will get
   errors unless the .ctors and .dtors sections are marked as writable
   via the SHF_WRITE attribute.)  */

#ifndef CTORS_SECTION_ASM_OP
#define CTORS_SECTION_ASM_OP	".section\t.ctors,\"aw\""
#endif
#ifndef DTORS_SECTION_ASM_OP
#define DTORS_SECTION_ASM_OP	".section\t.dtors,\"aw\""
#endif

#ifdef OBJECT_FORMAT_ELF

/*  Declare a pointer to void function type.  */
typedef void (*func_ptr) (void);
#define STATIC static

#else  /* OBJECT_FORMAT_ELF */

#include "gbl-ctors.h"

#define STATIC

#endif /* OBJECT_FORMAT_ELF */

#ifdef CRT_BEGIN

#ifdef INIT_SECTION_ASM_OP

#ifdef OBJECT_FORMAT_ELF

/* Declare the __dso_handle variable.  It should have a unique value
   in every shared-object; in a main program its value is zero.  */

#ifdef CRTSTUFFS_O
void *__dso_handle = &__dso_handle;
#else
void *__dso_handle = 0;
#endif

/* The __cxa_finalize function may not be available so we use only a
   weak declaration.  */
extern void __cxa_finalize (void *) TARGET_ATTRIBUTE_WEAK;

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
   same particular root executable or shared library file.

   On some systems, this routine is run more than once from the .fini,
   when exit is called recursively, so we arrange to remember where in
   the list we left off processing, and we resume at that point,
   should we be re-invoked.  */

static char __EH_FRAME_BEGIN__[];
static func_ptr __DTOR_LIST__[];
static void
__do_global_dtors_aux (void)
{
  static func_ptr *p = __DTOR_LIST__ + 1;
  static int completed = 0;

  if (completed)
    return;

#ifdef CRTSTUFFS_O
  if (__cxa_finalize)
    __cxa_finalize (__dso_handle);
#endif

  while (*p)
    {
      p++;
      (*(p-1)) ();
    }

#ifdef EH_FRAME_SECTION_ASM_OP
  if (__deregister_frame_info)
    __deregister_frame_info (__EH_FRAME_BEGIN__);
#endif
  completed = 1;
}


/* Stick a call to __do_global_dtors_aux into the .fini section.  */

static void __attribute__ ((__unused__))
fini_dummy (void)
{
  asm (FINI_SECTION_ASM_OP);
  __do_global_dtors_aux ();
#ifdef FORCE_FINI_SECTION_ALIGN
  FORCE_FINI_SECTION_ALIGN;
#endif
  asm (TEXT_SECTION_ASM_OP);
}

#ifdef EH_FRAME_SECTION_ASM_OP
/* Stick a call to __register_frame_info into the .init section.  For some
   reason calls with no arguments work more reliably in .init, so stick the
   call in another function.  */

static void
frame_dummy (void)
{
  static struct object object;
  if (__register_frame_info)
    __register_frame_info (__EH_FRAME_BEGIN__, &object);
}

static void __attribute__ ((__unused__))
init_dummy (void)
{
  asm (INIT_SECTION_ASM_OP);
  frame_dummy ();
#ifdef FORCE_INIT_SECTION_ALIGN
  FORCE_INIT_SECTION_ALIGN;
#endif
  asm (TEXT_SECTION_ASM_OP);
}
#endif /* EH_FRAME_SECTION_ASM_OP */

#else  /* OBJECT_FORMAT_ELF */

/* The function __do_global_ctors_aux is compiled twice (once in crtbegin.o
   and once in crtend.o).  It must be declared static to avoid a link
   error.  Here, we define __do_global_ctors as an externally callable
   function.  It is externally callable so that __main can invoke it when
   INVOKE__main is defined.  This has the additional effect of forcing cc1
   to switch to the .text section.  */

static void __do_global_ctors_aux (void);
void
__do_global_ctors (void)
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

/* On some svr4 systems, the initial .init section preamble code provided in
   crti.o may do something, such as bump the stack, which we have to 
   undo before we reach the function prologue code for __do_global_ctors 
   (directly below).  For such systems, define the macro INIT_SECTION_PREAMBLE
   to expand into the code needed to undo the actions of the crti.o file.  */

#ifdef INIT_SECTION_PREAMBLE
  INIT_SECTION_PREAMBLE;
#endif

/* A routine to invoke all of the global constructors upon entry to the
   program.  We put this into the .init section (for systems that have
   such a thing) so that we can properly perform the construction of
   file-scope static-storage C++ objects within shared libraries.   */

static void
__do_global_ctors_aux (void)	/* prologue goes in .init section */
{
#ifdef FORCE_INIT_SECTION_ALIGN
  FORCE_INIT_SECTION_ALIGN;	/* Explicit align before switch to .text */
#endif
  asm (TEXT_SECTION_ASM_OP);	/* don't put epilogue and body in .init */
  DO_GLOBAL_CTORS_BODY;
  atexit (__do_global_dtors);
}

#endif /* OBJECT_FORMAT_ELF */

#else /* defined(INIT_SECTION_ASM_OP) */

#ifdef HAS_INIT_SECTION
/* This case is used by the Irix 6 port, which supports named sections but
   not an SVR4-style .fini section.  __do_global_dtors can be non-static
   in this case because we protect it with -hidden_symbol.  */

static char __EH_FRAME_BEGIN__[];
static func_ptr __DTOR_LIST__[];
void
__do_global_dtors (void)
{
  func_ptr *p;
  for (p = __DTOR_LIST__ + 1; *p; p++)
    (*p) ();

#ifdef EH_FRAME_SECTION_ASM_OP
  if (__deregister_frame_info)
    __deregister_frame_info (__EH_FRAME_BEGIN__);
#endif
}

#ifdef EH_FRAME_SECTION_ASM_OP
/* Define a function here to call __register_frame.  crtend.o is linked in
   after libgcc.a, and hence can't call libgcc.a functions directly.  That
   can lead to unresolved function references.  */
void
__frame_dummy (void)
{
  static struct object object;
  if (__register_frame_info)
    __register_frame_info (__EH_FRAME_BEGIN__, &object);
}
#endif
#endif

#endif /* defined(INIT_SECTION_ASM_OP) */

/* Force cc1 to switch to .data section.  */
static func_ptr force_to_data[0] __attribute__ ((__unused__)) = { };

/* NOTE:  In order to be able to support SVR4 shared libraries, we arrange
   to have one set of symbols { __CTOR_LIST__, __DTOR_LIST__, __CTOR_END__,
   __DTOR_END__ } per root executable and also one set of these symbols
   per shared library.  So in any given whole process image, we may have
   multiple definitions of each of these symbols.  In order to prevent
   these definitions from conflicting with one another, and in order to
   ensure that the proper lists are used for the initialization/finalization
   of each individual shared library (respectively), we give these symbols
   only internal (i.e. `static') linkage, and we also make it a point to
   refer to only the __CTOR_END__ symbol in crtend.o and the __DTOR_LIST__
   symbol in crtbegin.o, where they are defined.  */

/* The -1 is a flag to __do_global_[cd]tors
   indicating that this table does not start with a count of elements.  */
#ifdef CTOR_LIST_BEGIN
CTOR_LIST_BEGIN;
#else
asm (CTORS_SECTION_ASM_OP);	/* cc1 doesn't know that we are switching! */
STATIC func_ptr __CTOR_LIST__[1] __attribute__ ((__unused__))
  = { (func_ptr) (-1) };
#endif

#ifdef DTOR_LIST_BEGIN
DTOR_LIST_BEGIN;
#else
asm (DTORS_SECTION_ASM_OP);	/* cc1 doesn't know that we are switching! */
STATIC func_ptr __DTOR_LIST__[1] = { (func_ptr) (-1) };
#endif

#ifdef EH_FRAME_SECTION_ASM_OP
/* Stick a label at the beginning of the frame unwind info so we can register
   and deregister it with the exception handling library code.  */

asm (EH_FRAME_SECTION_ASM_OP);
#ifdef INIT_SECTION_ASM_OP
STATIC
#endif
char __EH_FRAME_BEGIN__[] = { };
#endif /* EH_FRAME_SECTION_ASM_OP */

#endif /* defined(CRT_BEGIN) */

#ifdef CRT_END

#ifdef INIT_SECTION_ASM_OP

#ifdef OBJECT_FORMAT_ELF

static func_ptr __CTOR_END__[];
static void
__do_global_ctors_aux (void)
{
  func_ptr *p;
  for (p = __CTOR_END__ - 1; *p != (func_ptr) -1; p--)
    (*p) ();
}

/* Stick a call to __do_global_ctors_aux into the .init section.  */

static void __attribute__ ((__unused__))
init_dummy (void)
{
  asm (INIT_SECTION_ASM_OP);
  __do_global_ctors_aux ();
#ifdef FORCE_INIT_SECTION_ALIGN
  FORCE_INIT_SECTION_ALIGN;
#endif
  asm (TEXT_SECTION_ASM_OP);

/* This is a kludge. The i386 GNU/Linux dynamic linker needs ___brk_addr,
   __environ and atexit (). We have to make sure they are in the .dynsym
   section. We accomplish it by making a dummy call here. This
   code is never reached.  */
 
#if defined(__linux__) && defined(__PIC__) && defined(__i386__)
  {
    extern void *___brk_addr;
    extern char **__environ;

    ___brk_addr = __environ;
    atexit (0);
  }
#endif
}

#else  /* OBJECT_FORMAT_ELF */

/* Stick the real initialization code, followed by a normal sort of
   function epilogue at the very end of the .init section for this
   entire root executable file or for this entire shared library file.

   Note that we use some tricks here to get *just* the body and just
   a function epilogue (but no function prologue) into the .init
   section of the crtend.o file.  Specifically, we switch to the .text
   section, start to define a function, and then we switch to the .init
   section just before the body code.

   Earlier on, we put the corresponding function prologue into the .init
   section of the crtbegin.o file (which will be linked in first).

   Note that we want to invoke all constructors for C++ file-scope static-
   storage objects AFTER any other possible initialization actions which
   may be performed by the code in the .init section contributions made by
   other libraries, etc.  That's because those other initializations may
   include setup operations for very primitive things (e.g. initializing
   the state of the floating-point coprocessor, etc.) which should be done
   before we start to execute any of the user's code.  */

static void
__do_global_ctors_aux (void)	/* prologue goes in .text section */
{
  asm (INIT_SECTION_ASM_OP);
  DO_GLOBAL_CTORS_BODY;
  atexit (__do_global_dtors);
}				/* epilogue and body go in .init section */

#ifdef FORCE_INIT_SECTION_ALIGN
FORCE_INIT_SECTION_ALIGN;
#endif

asm (TEXT_SECTION_ASM_OP);

#endif /* OBJECT_FORMAT_ELF */

#else /* defined(INIT_SECTION_ASM_OP) */

#ifdef HAS_INIT_SECTION
/* This case is used by the Irix 6 port, which supports named sections but
   not an SVR4-style .init section.  __do_global_ctors can be non-static
   in this case because we protect it with -hidden_symbol.  */
static func_ptr __CTOR_END__[];
#ifdef EH_FRAME_SECTION_ASM_OP
extern void __frame_dummy (void);
#endif
void
__do_global_ctors (void)
{
  func_ptr *p;
#ifdef EH_FRAME_SECTION_ASM_OP
  __frame_dummy ();
#endif
  for (p = __CTOR_END__ - 1; *p != (func_ptr) -1; p--)
    (*p) ();
}
#endif

#endif /* defined(INIT_SECTION_ASM_OP) */

/* Force cc1 to switch to .data section.  */
static func_ptr force_to_data[0] __attribute__ ((__unused__)) = { };

/* Put a word containing zero at the end of each of our two lists of function
   addresses.  Note that the words defined here go into the .ctors and .dtors
   sections of the crtend.o file, and since that file is always linked in
   last, these words naturally end up at the very ends of the two lists
   contained in these two sections.  */

#ifdef CTOR_LIST_END
CTOR_LIST_END;
#else
asm (CTORS_SECTION_ASM_OP);	/* cc1 doesn't know that we are switching! */
STATIC func_ptr __CTOR_END__[1] = { (func_ptr) 0 };
#endif

#ifdef DTOR_LIST_END
DTOR_LIST_END;
#else
asm (DTORS_SECTION_ASM_OP);	/* cc1 doesn't know that we are switching! */
STATIC func_ptr __DTOR_END__[1] __attribute__ ((__unused__))
  = { (func_ptr) 0 };
#endif

#ifdef EH_FRAME_SECTION_ASM_OP
/* Terminate the frame unwind info section with a 4byte 0 as a sentinel;
   this would be the 'length' field in a real FDE.  */

typedef unsigned int ui32 __attribute__ ((mode (SI)));
asm (EH_FRAME_SECTION_ASM_OP);
STATIC ui32 __FRAME_END__[] __attribute__ ((__unused__)) = { 0 };
#endif /* EH_FRAME_SECTION */

#endif /* defined(CRT_END) */

#else  /* OBJECT_FORMAT_MACHO */

/* For Mach-O format executables, we assume that the system's runtime is
   smart enough to handle constructors and destructors, but doesn't have
   an init section (if it can't even handle constructors/destructors
   you should be using INVOKE__main, not crtstuff). All we need to do
   is install/deinstall the frame information for exceptions. We do this
   by putting a constructor in crtbegin.o and a destructor in crtend.o.

   crtend.o also puts in the terminating zero in the frame information
   segment. */

/* The crtstuff for other object formats use the symbol __EH_FRAME_BEGIN__
   to figure out the start of the exception frame, but here we use
   getsectbynamefromheader to find this value. Either method would work,
   but this method avoids creating any global symbols, which seems
   cleaner. */

#include <mach-o/ldsyms.h>
extern const struct section *
  getsectbynamefromheader (const struct mach_header *,
			   const char *, const char *);

#ifdef CRT_BEGIN

static void __reg_frame_ctor (void) __attribute__ ((constructor));

static void
__reg_frame_ctor (void)
{
  static struct object object;
  const struct section *eh_frame;

  eh_frame = getsectbynamefromheader (&_mh_execute_header,
				      "__TEXT", "__eh_frame");
  __register_frame_info ((void *) eh_frame->addr, &object);
}

#endif /* CRT_BEGIN */

#ifdef CRT_END

static void __dereg_frame_dtor (void) __attribute__ ((destructor));

static void
__dereg_frame_dtor (void)
{
  const struct section *eh_frame;

  eh_frame = getsectbynamefromheader (&_mh_execute_header,
				      "__TEXT", "__eh_frame");
  __deregister_frame_info ((void *) eh_frame->addr);
}

/* Terminate the frame section with a final zero. */

/* Force cc1 to switch to .data section.  */
static void * force_to_data[0] __attribute__ ((__unused__)) = { };

typedef unsigned int ui32 __attribute__ ((mode (SI)));
asm (EH_FRAME_SECTION_ASM_OP);
static ui32 __FRAME_END__[] __attribute__ ((__unused__)) = { 0 };

#endif /* CRT_END */

#endif /* OBJECT_FORMAT_MACHO */

