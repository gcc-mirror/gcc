/* Definitions for embedded ia64-elf target.

Copyright (C) 2000-2014 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

/* A C string constant that tells the GCC driver program options to pass to
   the assembler.  It can also specify how to translate options you give to GNU
   CC into options for GCC to pass to the assembler.  */

#if ((TARGET_CPU_DEFAULT | TARGET_DEFAULT) & MASK_GNU_AS) != 0
/* GNU AS.  */
#undef  ASM_EXTRA_SPEC
#define ASM_EXTRA_SPEC \
  "%{mno-gnu-as:-N so} %{!mno-gnu-as:-x}"
#else
/* Intel ias.  */
#undef  ASM_SPEC
#define ASM_SPEC \
  "%{!mgnu-as:-N so} %{mgnu-as:-x} %{mconstant-gp:-M const_gp}\
   %{mauto-pic:-M no_plabel}"
#endif

/* A C string constant that tells the GCC driver program options to pass to
   the linker.  It can also specify how to translate options you give to GCC
   into options for GCC to pass to the linker.  */

/* The Intel linker does not support dynamic linking, so we need -dn.
   The Intel linker gives annoying messages unless -N so is used.  */
#if ((TARGET_CPU_DEFAULT | TARGET_DEFAULT) & MASK_GNU_LD) != 0
/* GNU LD.  */
#define LINK_SPEC "%{mno-gnu-ld:-dn -N so}"
#else
/* Intel ild.  */
#define LINK_SPEC "%{!mgnu-ld:-dn -N so}"
#endif

/* elfos.h does not link with crti.o/crtn.o.  We override elfos.h so
   that we can use the standard ELF Unix method.  */
#undef  ENDFILE_SPEC
#define ENDFILE_SPEC "crtend.o%s crtn.o%s"

#undef	STARTFILE_SPEC
#define STARTFILE_SPEC "%{!shared: \
			 %{!symbolic: \
			  %{pg:gcrt0.o%s}%{!pg:%{p:mcrt0.o%s}%{!p:crt0.o%s}}}}\
			crti.o%s crtbegin.o%s"

/* End of elf.h */
