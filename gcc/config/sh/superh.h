/* Definitions of target machine for gcc for Super-H using sh-superh-elf.
   Copyright (C) 2001 Free Software Foundation, Inc.

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


/* This header file is used when the vendor name is set to 'superh'.
   It configures the compiler for SH4 only and switches the default
   endianess to little (although big endian is still available).
   It also configures the spec file to the default board configuration
   but in such a way that it can be overridden by a boardspecs file
   (using the -specs= option). This file is expected to disable the
   defaults and provide options --defsym _start and --defsym _stack
   which are required by the SuperH configuration of GNU ld.

   This file is intended to overide sh.h */


#ifndef _SUPERH_H
#define _SUPERH_H
#endif


#undef TARGET_VERSION
#define TARGET_VERSION fprintf (stderr, " (SuperH SH special %s)", __DATE__);


/* We override TARGET_PROCESSOR_SWITCHES in order to remove all the unrequired cpu options
   and add options for all the SuperH CPU variants:
   -m4-100  is an alias for -m4.
   -m4-200  is an alias for -m4.
   -m4-400  is an alias for -m4-nofpu and passes -isa=sh4-nommu-nofpu to the assembler.
   -m4-500  is an alias for -m4-nofpu and passes -isa=sh4-nofpu to the assembler.  */
#undef TARGET_PROCESSOR_SWITCHES
#define TARGET_PROCESSOR_SWITCHES \
  {"4-500",	TARGET_NONE, "SH4 500 series (FPU-less)" }, \
  {"4-500",	SELECT_SH4_NOFPU, "" }, \
  {"4-400",	TARGET_NONE, "SH4 400 series (MMU/FPU-less)" },	\
  {"4-400",	SELECT_SH4_NOFPU, "" }, \
  {"4-200-single-only",	TARGET_NONE, "SH4 200 series with double = float (SH3e ABI)" },	\
  {"4-200-single-only",	SELECT_SH4_SINGLE_ONLY, "" }, \
  {"4-200-single",	TARGET_NONE, "SH4 200 series with single precision pervading" }, \
  {"4-200-single",	SELECT_SH4_SINGLE, "" }, \
  {"4-200-nofpu",	TARGET_NONE, "SH4 200 series using soft floating point" }, \
  {"4-200-nofpu",	SELECT_SH4_NOFPU, "" }, \
  {"4-200",	TARGET_NONE, "SH4 200 series" }, \
  {"4-200",	SELECT_SH4_NOFPU, "" }, \
  {"4-100-single-only",	TARGET_NONE, "SH4 100 series with double = float (SH3e ABI)" },	\
  {"4-100-single-only",	SELECT_SH4_SINGLE_ONLY, "" }, \
  {"4-100-single",	TARGET_NONE, "SH4 100 series with single precision pervading" }, \
  {"4-100-single",	SELECT_SH4_SINGLE, "" }, \
  {"4-100-nofpu",	TARGET_NONE, "SH4 100 series using soft floating point" }, \
  {"4-100-nofpu",	SELECT_SH4_NOFPU, "" }, \
  {"4-100",	TARGET_NONE, "SH4 100 series" }, \
  {"4-100",	SELECT_SH4_NOFPU, "" }, \
  {"4-single-only",	TARGET_NONE, "Generic SH4 with double = float (SH3e ABI)" }, \
  {"4-single-only",	SELECT_SH4_SINGLE_ONLY, "" }, \
  {"4-single",	TARGET_NONE, "Generic SH4 with single precision pervading" }, \
  {"4-single",	SELECT_SH4_SINGLE, "" }, \
  {"4-nofpu",	TARGET_NONE, "Generic SH4 using soft floating point" },	\
  {"4-nofpu",	SELECT_SH4_NOFPU, "" }, \
  {"4",	        TARGET_NONE, "Generic SH4 (default)" },	\
  {"4",	        SELECT_SH4, "" }


/* Provide the -mboard= option used by the boardspecs file */
#undef SUBTARGET_OPTIONS
#define SUBTARGET_OPTIONS \
  { "board=",   &boardtype, "Board name [and momory region].", 0 }, \
  { "runtime=", &osruntime, "Runtime name.", 0 }, \

/* These are required by the mboard= option and runtime= option
   and are defined in sh.c but are not used anywhere */
extern const char * boardtype;
extern const char * osruntime;


/* Override the linker spec strings to use the new emulation
   The specstrings are concatenated as follows
   LINK_EMUL_PREFIX.(''|'32'|'64'|LINK_DEFAULT_CPU_EMUL).SUBTARGET_LINK_EMUL_SUFFIX
*/
#undef LINK_EMUL_PREFIX
#undef SUBTARGET_LINK_EMUL_SUFFIX

#define LINK_EMUL_PREFIX "superh"
#define SUBTARGET_LINK_EMUL_SUFFIX ""

/* Add the SUBTARGET_LINK_SPEC to add the board and runtime support and
   change the endianness */
#undef SUBTARGET_LINK_SPEC
#if  TARGET_ENDIAN_DEFAULT == LITTLE_ENDIAN_BIT
#define SUBTARGET_LINK_SPEC "%(board_link) %(ldruntime) %{ml|!mb:-EL}%{mb:-EB}"
#else
#define SUBTARGET_LINK_SPEC "%(board_link) %(ldruntime) %{ml:-EL}%{mb|!ml:-EB}"
#endif


/* This is used by the link spec if the boardspecs file is not used (for whatever reason).
   If the boardspecs file overrides this then an alternative can be used. */
#undef SUBTARGET_EXTRA_SPECS
#define SUBTARGET_EXTRA_SPECS \
{ "board_link", "--defsym _start=0x1000 --defsym _stack=0x30000" }, \
{ "asruntime", "" }, \
{ "cppruntime", "-D__GDB_SIM__" }, \
{ "cc1runtime", "" }, \
{ "ldruntime", "" }, \
{ "libruntime", "-lc -lgloss" }


/* Set the SUBTARGET_CPP_SPEC to define __EMBEDDED_CROSS__ which has an effect
   on newlib and provide the runtime support */
#undef SUBTARGET_CPP_SPEC
#define SUBTARGET_CPP_SPEC \
"-D__EMBEDDED_CROSS__ %{m4-100*:-D__SH4_100__} %{m4-200*:-D__SH4_200__} %{m4-400:-D__SH4_400__} %{m4-500:-D__SH4_500__} \
%(cppruntime)"

/* Override the SUBTARGET_ASM_SPEC to add the runtime support */
#undef SUBTARGET_ASM_SPEC
#define SUBTARGET_ASM_SPEC "%{m4-100*|m4-200*:-isa=sh4} %{m4-400:-isa=sh4-nommu-nofpu} %{m4-500:-isa=sh4-nofpu} %(asruntime)"

/* Override the SUBTARGET_ASM_RELAX_SPEC so it doesn't interfere with the
   runtime support by adding -isa=sh4 in the wrong place.  */
#undef SUBTARGET_ASM_RELAX_SPEC
#define SUBTARGET_ASM_RELAX_SPEC "%{!m4-100*:%{!m4-200*:%{!m4-400:%{!m4-500:-isa=sh4}}}}"

/* Create the CC1_SPEC to add the runtime support */
#undef CC1_SPEC
#define CC1_SPEC "%(cc1runtime)"

#undef CC1PLUS_SPEC
#define CC1PLUS_SPEC "%(cc1runtime)"


/* Override the LIB_SPEC to add the runtime support */
#undef LIB_SPEC
#define LIB_SPEC "%{!shared:%{!symbolic:%(libruntime) -lc}} %{pg:-lprofile -lc}"
