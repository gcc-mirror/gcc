/* Definitions of target machine for GNU compiler, for sparclite 86x w/o FPU.
   Copyright (C) 1998, 1999, 2000 Free Software Foundation, Inc.
   Contributed by Stan Cox (scox@cygnus.com).

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

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-D__sparc__ -D__sparclite86x__ -Acpu=sparc -Amachine=sparc"

/* Default to dwarf2 in ELF.  */

#define DWARF_DEBUGGING_INFO 1
#define DWARF2_DEBUGGING_INFO 1

#undef PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE DWARF2_DEBUG

#undef TARGET_VERSION
#define TARGET_VERSION fprintf (stderr, " (sparclite 86x)");

/* Enable app-regs and epilogue options.  Do not enable the fpu.  */

#undef TARGET_DEFAULT
#define TARGET_DEFAULT MASK_APP_REGS

#undef ASM_SPEC
#define ASM_SPEC "%{v:-V} %{mlittle-endian-data:--little-endian-data} %(asm_cpu)"

/* US Software GOFAST library support.  */
#undef INIT_SUBTARGET_OPTABS
#define INIT_SUBTARGET_OPTABS INIT_GOFAST_OPTABS      

#undef STARTFILE_SPEC
#define STARTFILE_SPEC "crti.o%s crtbegin.o%s"

#undef LINK_SPEC
#define LINK_SPEC "%{v:-V} %{mlittle-endian-data:-EL}"

#undef BYTES_BIG_ENDIAN
#define BYTES_BIG_ENDIAN (! TARGET_LITTLE_ENDIAN_DATA)
#undef WORDS_BIG_ENDIAN
#define WORDS_BIG_ENDIAN (! TARGET_LITTLE_ENDIAN_DATA)

/* Use __main method of constructor invocation */
#undef INIT_SECTION_ASM_OP

#define TARGET_LITTLE_ENDIAN_DATA (target_flags & MASK_LITTLE_ENDIAN)
#undef  SUBTARGET_SWITCHES
#define SUBTARGET_SWITCHES \
    { "little-endian-data",              MASK_LITTLE_ENDIAN,	N_("Use little-endian byte order for data")},
