/* Definitions of target machine for GNU compiler.  ARM RISCiX 1.1x version.
   Copyright (C) 1993, 1995 Free Software Foundation, Inc.
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

/* RISCix 1.1x is basically the same as 1.2x except that it doesn't have
   symrename or atexit. */

/* Translation to find startup files.  On RISCiX boxes, gcrt0.o is in
   /usr/lib.  */
#define STARTFILE_SPEC  \
  "%{pg:/usr/lib/gcrt0.o%s}%{!pg:%{p:mcrt0.o%s}%{!p:crt0.o%s}}"

#ifndef CPP_PREDEFINES
#define CPP_PREDEFINES  "-Darm -Driscix -Dunix -Asystem(unix) -Acpu(arm) -Amachine(arm)"
#endif

#ifndef CPP_SPEC
#define CPP_SPEC "%{m6:-D__arm6__} %{!ansi: -D_BSD_C}"
#endif

/* Riscix 1.1 doesn't have X/OPEN support, so only accept -mbsd (but ignore
   it).  
   By not having -mxopen and -mno-symrename, we get warning messages,
   but everything still compiles.  */
/* None of these is actually used in cc1, so they modify bit 31 */
#define SUBTARGET_SWITCHES \
{"bsd", 0x80000000}, 
    

/* Run-time Target Specification.  */
#define TARGET_VERSION  \
  fputs (" (ARM/RISCiX)", stderr);

/* This is used in ASM_FILE_START */
#define ARM_OS_NAME "RISCiX"

#ifdef riscos
#define TARGET_WHEN_DEBUGGING  3
#else
#define TARGET_WHEN_DEBUGGING  1
#endif

/* 'char' is signed by default on RISCiX, unsigned on RISCOS.  */
#ifdef riscos
#define DEFAULT_SIGNED_CHAR  0
#else
#define DEFAULT_SIGNED_CHAR  1
#endif

/* Define this if the target system supports the function atexit form the
   ANSI C standard.  If this is not defined, and INIT_SECTION_ASM_OP is not
   defined, a default exit function will be provided to support C++.  
   The man page only describes on_exit, but atexit is also there.  
   This seems to be missing in early versions. */
/*#define HAVE_ATEXIT 1 */
/* Some systems use __main in a way incompatible with its use in gcc, in these
   cases use the macros NAME__MAIN to give a quoted symbol and SYMBOL__MAIN to
   give the same symbol without quotes for an alternative entry point.  You
   must define both, or neither. */
#ifndef NAME__MAIN
#define NAME__MAIN "__gccmain"
#define SYMBOL__MAIN __gccmain
#endif

#include "arm/arm.h"

/* The native RISCiX assembler does not support stabs of any kind; because
   the native assembler is not used by the compiler, Acorn didn't feel it was
   necessary to put them in!  */

#ifdef DBX_DEBUGGING_INFO
#undef DBX_DEBUGGING_INFO
#endif
