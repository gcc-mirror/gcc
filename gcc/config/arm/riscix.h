/* Definitions of target machine for GNU compiler.  ARM RISCiX version.
   Copyright (C) 1993, 1994 Free Software Foundation, Inc.
   Contributed by Richard Earnshaw (rwe11@cl.cam.ac.uk), based on original
	      work by Pieter `Tiggr' Schoenmakers (rcpieter@win.tue.nl)
   	      and Martin Simmons (@harleqn.co.uk).

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

/* Translation to find startup files.  On RISC iX boxes,
   crt0, mcrt0 and gcrt0.o are in /usr/lib.  */
#define STARTFILE_SPEC  "\
  %{pg:/usr/lib/gcrt0.o%s}\
  %{!pg:%{p:/usr/lib/mcrt0.o%s}\
        %{!p:/usr/lib/crt0.o%s}}"

/* RISC iX has no concept of -lg */
/* If -static is specified then link with -lc_n */

#ifndef LIB_SPEC
#define LIB_SPEC "\
  %{g*:-lg}\
  %{!p:%{!pg:%{!static:-lc}%{static:-lc_n}}}\
  %{p:-lc_p}\
  %{pg:-lc_p}"
#endif
  
/* The RISC iX assembler never deletes any symbols from the object module;
   and, by default, ld doesn't either.  -X causes local symbols starting
   with 'L' to be deleted, which is what we want.  */
#ifndef LINK_SPEC
#define LINK_SPEC "-X"
#endif

#ifndef CPP_PREDEFINES
#define CPP_PREDEFINES  \
    "-Darm -Driscix -Dunix -Asystem(unix) -Acpu(arm) -Amachine(arm)"
#endif

#ifndef CPP_SPEC
#define CPP_SPEC "%{m6:-D__arm6__} \
	%{mbsd:%{pedantic:%e-mbsd and -pedantic incompatible} -D_BSD_C} \
	%{mxopen:%{mbsd:%e-mbsd and -mxopen incompatible} 		\
	  %{pedantic:%e-mxopen and -pedantic incompatible} -D_XOPEN_C}  \
	%{!mbsd:%{!mxopen:%{!ansi: -D_BSD_C}}}"
#endif

/* RISCiX has some wierd symbol name munging, that is done to the object module
   after assembly, which enables multiple libraries to be supported within
   one (possibly shared) library.  It basically changes the symbol name of
   certain symbols (for example _bcopy is converted to _$bcopy if using BSD)
   Symrename's parameters are determined as follows:
     -mno-symrename	Don't run symrename
     -mbsd	symrename -BSD <file>
     -mxopen	symrename -XOPEN <file>
     -ansi	symrename - <file>
     <none>	symrename -BSD <file>
 */

#ifndef ASM_FINAL_SPEC
#if !defined (CROSS_COMPILE)
#define ASM_FINAL_SPEC "\
%{!mno-symrename: \
	\n /usr/bin/symrename \
	-%{mbsd:%{pedantic:%e-mbsd and -pedantic incompatible}BSD}\
%{mxopen:%{mbsd:%e-mbsd and -mxopen incompatible}\
%{pedantic:%e-mxopen and -pedantic incompatible}XOPEN}\
%{!mbsd:%{!mxopen:%{!ansi:BSD}}} %{c:%{o*:%*}%{!o*:%b.o}}%{!c:%U.o}}"
#endif
#endif

/* None of these is actually used in cc1, so they modify bit 31 */
#define ARM_EXTRA_TARGET_SWITCHES \
{"bsd", 0x80000000}, {"xopen", 0x80000000}, {"no-symrename", 0x80000000},

    

/* Run-time Target Specification.  */
#define TARGET_VERSION  \
  fputs (" (ARM/RISCiX)", stderr);

/* This is used in ASM_FILE_START */
#define ARM_OS_NAME "RISCiX"

/* Unsigned chars produces much better code than signed.  */
#define DEFAULT_SIGNED_CHAR  0

/* Define this if the target system supports the function atexit from the
   ANSI C standard.  If this is not defined, and INIT_SECTION_ASM_OP is not
   defined, a default exit function will be provided to support C++.  
   The man page only describes on_exit, but atexit is also there.  */
#define HAVE_ATEXIT 1

/* Some systems use __main in a way incompatible with its use in gcc, in these
   cases use the macros NAME__MAIN to give a quoted symbol and SYMBOL__MAIN to
   give the same symbol without quotes for an alternative entry point.  You
   must define both, or neither. */
#ifndef NAME__MAIN
#define NAME__MAIN "__gccmain"
#define SYMBOL__MAIN __gccmain
#endif

/* size_t is "unsigned int" in RISCiX */
#define SIZE_TYPE "unsigned int"

/* ptrdiff_t is "int" in RISCiX */
#define PTRDIFF_TYPE "int"

/* Maths operation domain error number, EDOM */
#define TARGET_EDOM 33
#include "arm/arm.h"

/* The native RISCiX assembler does not support stabs of any kind; because
   the native assembler is not used by the compiler, Acorn didn't feel it was
   necessary to put them in!  */

#ifdef DBX_DEBUGGING_INFO
#undef DBX_DEBUGGING_INFO
#endif
