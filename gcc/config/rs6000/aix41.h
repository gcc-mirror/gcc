/* Definitions of target machine for GNU compiler,
   for IBM RS/6000 POWER running AIX version 4.1.
   Copyright (C) 1994, 95-8, 1999 Free Software Foundation, Inc.
   Contributed by David Edelsohn (edelsohn@gnu.org).

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


/* Enable AIX XL compiler calling convention breakage compatibility.  */
#define MASK_XL_CALL		0x40000000
#define	TARGET_XL_CALL		(target_flags & MASK_XL_CALL)
#undef  SUBTARGET_SWITCHES
#define SUBTARGET_SWITCHES		\
  {"xl-call", 		MASK_XL_CALL},	\
  {"no-xl-call",	- MASK_XL_CALL}, \
  {"threads",		0},		\
  {"pe",		0},

#include "rs6000/rs6000.h"

#undef ASM_SPEC
#define ASM_SPEC "-u %(asm_cpu)"

/* Common ASM definitions used by ASM_SPEC amonst the various targets
   for handling -mcpu=xxx switches.  */
#undef ASM_CPU_SPEC
#define ASM_CPU_SPEC \
"%{!mcpu*: \
  %{mpower: %{!mpower2: -mpwr}} \
  %{mpower2: -mpwr2} \
  %{mpowerpc*: -mppc} \
  %{!mpower*: %{!mpowerpc*: %(asm_default)}}} \
%{mcpu=common: -mcom} \
%{mcpu=power: -mpwr} \
%{mcpu=power2: -mpwr2} \
%{mcpu=powerpc: -mppc} \
%{mcpu=rios: -mpwr} \
%{mcpu=rios1: -mpwr} \
%{mcpu=rios2: -mpwr2} \
%{mcpu=rsc: -mpwr} \
%{mcpu=rsc1: -mpwr} \
%{mcpu=403: -mppc} \
%{mcpu=505: -mppc} \
%{mcpu=601: -m601} \
%{mcpu=602: -mppc} \
%{mcpu=603: -m603} \
%{mcpu=603e: -m603} \
%{mcpu=604: -m604} \
%{mcpu=604e: -m604} \
%{mcpu=620: -mppc} \
%{mcpu=821: -mppc} \
%{mcpu=860: -mppc}"

#undef	ASM_DEFAULT_SPEC
#define ASM_DEFAULT_SPEC "-mcom"

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-D_IBMR2 -D_POWER -D_AIX -D_AIX32 -D_AIX41 \
-D_LONG_LONG -Asystem(unix) -Asystem(aix)"

#undef CPP_SPEC
#define CPP_SPEC "%{posix: -D_POSIX_SOURCE}\
   %{mpe: -I/usr/lpp/ppe.poe/include}\
   %{mthreads: -D_THREAD_SAFE}\
   %(cpp_cpu)"

/* Common CPP definitions used by CPP_SPEC among the various targets
   for handling -mcpu=xxx switches.  */
#undef CPP_CPU_SPEC
#define CPP_CPU_SPEC \
"%{!mcpu*: \
  %{mpower: %{!mpower2: -D_ARCH_PWR}} \
  %{mpower2: -D_ARCH_PWR2} \
  %{mpowerpc*: -D_ARCH_PPC} \
  %{!mpower*: %{!mpowerpc*: %(cpp_default)}}} \
%{mcpu=common: -D_ARCH_COM} \
%{mcpu=power: -D_ARCH_PWR} \
%{mcpu=power2: -D_ARCH_PWR2} \
%{mcpu=powerpc: -D_ARCH_PPC} \
%{mcpu=rios: -D_ARCH_PWR} \
%{mcpu=rios1: -D_ARCH_PWR} \
%{mcpu=rios2: -D_ARCH_PWR2} \
%{mcpu=rsc: -D_ARCH_PWR} \
%{mcpu=rsc1: -D_ARCH_PWR} \
%{mcpu=403: -D_ARCH_PPC} \
%{mcpu=505: -D_ARCH_PPC} \
%{mcpu=601: -D_ARCH_PPC -D_ARCH_PWR} \
%{mcpu=602: -D_ARCH_PPC} \
%{mcpu=603: -D_ARCH_PPC} \
%{mcpu=603e: -D_ARCH_PPC} \
%{mcpu=604: -D_ARCH_PPC} \
%{mcpu=620: -D_ARCH_PPC} \
%{mcpu=821: -D_ARCH_PPC} \
%{mcpu=860: -D_ARCH_PPC}"

#undef	CPP_DEFAULT_SPEC
#define CPP_DEFAULT_SPEC "-D_ARCH_COM"

#undef TARGET_DEFAULT
#define TARGET_DEFAULT MASK_NEW_MNEMONICS

#undef PROCESSOR_DEFAULT
#define PROCESSOR_DEFAULT PROCESSOR_PPC601

/* Define this macro as a C expression for the initializer of an
   array of string to tell the driver program which options are
   defaults for this target and thus do not need to be handled
   specially when using `MULTILIB_OPTIONS'.

   Do not define this macro if `MULTILIB_OPTIONS' is not defined in
   the target makefile fragment or if none of the options listed in
   `MULTILIB_OPTIONS' are set by default.  *Note Target Fragment::.  */

#undef	MULTILIB_DEFAULTS
#define	MULTILIB_DEFAULTS { "mcpu=common" }

/* These are not necessary when we pass -u to the assembler, and undefining
   them saves a great deal of space in object files.  */

#undef ASM_OUTPUT_EXTERNAL
#undef ASM_OUTPUT_EXTERNAL_LIBCALL
#define ASM_OUTPUT_EXTERNAL(FILE, DECL, NAME)	\
{ rtx _symref = XEXP (DECL_RTL (DECL), 0);	\
  if ((TREE_CODE (DECL) == VAR_DECL		\
       || TREE_CODE (DECL) == FUNCTION_DECL)	\
      && (NAME)[strlen (NAME) - 1] != ']')	\
    {						\
      char *_name = (char *) permalloc (strlen (XSTR (_symref, 0)) + 5); \
      strcpy (_name, XSTR (_symref, 0));	\
      strcat (_name, TREE_CODE (DECL) == FUNCTION_DECL ? "[DS]" : "[RW]"); \
      XSTR (_symref, 0) = _name;		\
    }						\
}

#undef LIB_SPEC
#define LIB_SPEC "%{pg:-L/lib/profiled -L/usr/lib/profiled}\
   %{p:-L/lib/profiled -L/usr/lib/profiled} %{!shared:%{g*:-lg}}\
   %{mpe:-L/usr/lpp/ppe.poe/lib -lmpi -lvtd}\
   %{mthreads: -L/usr/lib/threads -lpthreads -lc_r /usr/lib/libc.a}\
   %{!mthreads: -lc}"

#undef LINK_SPEC
#define LINK_SPEC "-bpT:0x10000000 -bpD:0x20000000 %{!r:-btextro} -bnodelcsect\
   %{static:-bnso %(link_syscalls) } %{!shared: %{g*: %(link_libg) }}\
   %{shared:-bM:SRE %{!e:-bnoentry}}"

#undef STARTFILE_SPEC
#define STARTFILE_SPEC "%{!shared:\
   %{mpe:%{pg:/usr/lpp/ppe.poe/lib/gcrt0.o}\
         %{!pg:%{p:/usr/lpp/ppe.poe/lib/mcrt0.o}\
               %{!p:/usr/lpp/ppe.poe/lib/crt0.o}}}\
   %{!mpe:\
     %{mthreads:%{pg:gcrt0_r%O%s}%{!pg:%{p:mcrt0_r%O%s}%{!p:crt0_r%O%s}}}\
     %{!mthreads:%{pg:gcrt0%O%s}%{!pg:%{p:mcrt0%O%s}%{!p:crt0%O%s}}}}}"

