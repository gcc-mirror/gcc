/* Definitions of target machine for GNU compiler.  ARM RISCiX version.
   Copyright (C) 1993, 1994, 1995, 1997, 1999, 2000
   Free Software Foundation, Inc.
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
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

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
    "-Darm -Driscix -Dunix -Asystem=unix"
#endif


/* RISCiX has some weird symbol name munging, that is done to the object module
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

/* None of these is actually used in cc1.  If we don't define them in target
   switches cc1 complains about them.  For the sake of argument lets allocate
   bit 31 of target flags for such options.  */
#define SUBTARGET_SWITCHES						\
  {"bsd",	   0x80000000, N_("Do symbol renaming for BSD")},	\
  {"xopen",	   0x80000000, N_("Do symbol renaming for X/OPEN")},	\
  {"no-symrename", 0x80000000, N_("Don't do symbol renaming")},
    

/* Run-time Target Specification.  */
#define TARGET_VERSION  \
  fputs (" (ARM/RISCiX)", stderr);

/* This is used in ASM_FILE_START */
#define ARM_OS_NAME "RISCiX"

/* Unsigned chars produces much better code than signed.  */
#define DEFAULT_SIGNED_CHAR  0

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

/* Override the normal default CPU */
#define SUBTARGET_CPU_DEFAULT TARGET_CPU_arm2

/* r10 is reserved by RISCiX  */
#define SUBTARGET_CONDITIONAL_REGISTER_USAGE 	\
  fixed_regs[10] = 1;				\
  call_used_regs[10] = 1;

#include "arm/aout.h"

/* The RISCiX assembler does not understand .set */
#undef SET_ASM_OP

/* Add to CPP_SPEC, we want to add the right #defines when using the include
   files.  */
#define SUBTARGET_CPP_SPEC "\
	%{mbsd:%{pedantic:%e-mbsd and -pedantic incompatible} -D_BSD_C} \
	%{mxopen:%{mbsd:%e-mbsd and -mxopen incompatible} 		\
	  %{pedantic:%e-mxopen and -pedantic incompatible} -D_XOPEN_C}  \
	%{!mbsd:%{!mxopen:%{!ansi: -D_BSD_C}}}"

/* The native RISCiX assembler does not support stabs of any kind; because
   the native assembler is not used by the compiler, Acorn didn't feel it was
   necessary to put them in!  */

#ifdef DBX_DEBUGGING_INFO
#undef DBX_DEBUGGING_INFO
#endif
