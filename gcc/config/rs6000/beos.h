/* Definitions of target machine for GNU compiler, for BeOS.
   Copyright (C) 1997, 2000, 2001, 2002 Free Software Foundation, Inc.
   Contributed by Fred Fish (fnf@cygnus.com), based on aix41.h
   from David Edelsohn (edelsohn@npac.syr.edu).

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

#undef  TARGET_VERSION
#define TARGET_VERSION fprintf (stderr, " (BeOS/PowerPC)");

/* Enable AIX XL compiler calling convention breakage compatibility.  */
#define MASK_XL_CALL		0x40000000
#define	TARGET_XL_CALL		(target_flags & MASK_XL_CALL)
#undef  SUBTARGET_SWITCHES
#define SUBTARGET_SWITCHES		\
  {"xl-call", 		MASK_XL_CALL,					\
   N_("Always pass floating-point arguments in memory") },		\
  {"no-xl-call",	- MASK_XL_CALL,					\
   N_("Don't always pass floating-point arguments in memory") },	\
  {"threads",		0},						\
  {"pe",		0},

#undef ASM_SPEC
#define ASM_SPEC "-u %(asm_cpu)"

#undef CPP_PREDEFINES
/* __POWERPC__ must be defined for some header files */
#define CPP_PREDEFINES "-D__BEOS__ -D__POWERPC__ -Asystem=beos -Acpu=powerpc -Amachine=powerpc"

#undef CPP_SPEC
#define CPP_SPEC "%{posix: -D_POSIX_SOURCE} %(cpp_cpu)"

#undef	CPP_DEFAULT_SPEC
#define CPP_DEFAULT_SPEC "-D_ARCH_PPC"

/* This is the easiest way to disable use of gcc's builtin alloca,
   which in the current BeOS release (DR9) is a problem because of the
   relatively low default stack size of 256K with no way to expand it.
   So anything we compile for the BeOS target should not use the
   builtin alloca.  This also has the unwanted side effect of
   disabling all builtin functions though.  */

#undef CC1_SPEC
#define CC1_SPEC "%{!fbuiltin: -fno-builtin}"
#undef CC1PLUS_SPEC
#define CC1PLUS_SPEC "%{!fbuiltin: -fno-builtin}"

#undef	ASM_DEFAULT_SPEC
#define ASM_DEFAULT_SPEC "-mppc"

#undef TARGET_DEFAULT
#define TARGET_DEFAULT (MASK_POWERPC | MASK_NEW_MNEMONICS)

#undef PROCESSOR_DEFAULT
#define PROCESSOR_DEFAULT PROCESSOR_PPC603

/* Define this macro as a C expression for the initializer of an
   array of string to tell the driver program which options are
   defaults for this target and thus do not need to be handled
   specially when using `MULTILIB_OPTIONS'.

   Do not define this macro if `MULTILIB_OPTIONS' is not defined in
   the target makefile fragment or if none of the options listed in
   `MULTILIB_OPTIONS' are set by default.  *Note Target Fragment::.  */

#undef	MULTILIB_DEFAULTS
#define	MULTILIB_DEFAULTS { "mcpu=powerpc" }

/* These empty definitions get rid of the attempt to link in crt0.o
   and any libraries like libc.a.
   On BeOS the ld executable is actually a linker front end that first runs
   the GNU linker with the -r option to generate a relocatable XCOFF output
   file, and then runs Metrowork's linker (mwld) to generate a fully linked
   executable.  */

#undef LIB_SPEC
#define LIB_SPEC ""

#undef LINK_SPEC
#define LINK_SPEC ""

#undef STARTFILE_SPEC
#define STARTFILE_SPEC ""

/* Text to write out after a CALL that may be replaced by glue code by
   the loader.  */

#undef RS6000_CALL_GLUE
#define RS6000_CALL_GLUE "cror 15,15,15"

/* Struct alignments are done on 4 byte boundaries for all types.  */
#undef BIGGEST_FIELD_ALIGNMENT
#define BIGGEST_FIELD_ALIGNMENT 32

/* STANDARD_INCLUDE_DIR is the equivalent of "/usr/include" on UNIX.  */
#define STANDARD_INCLUDE_DIR	"/boot/develop/headers/posix"

/* SYSTEM_INCLUDE_DIR is the location for system specific, non-POSIX headers.  */
#define SYSTEM_INCLUDE_DIR	"/boot/develop/headers/be"
