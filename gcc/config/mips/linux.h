/* Definitions for MIPS running Linux-based GNU systems with ELF format.
   Copyright (C) 1998 Free Software Foundation, Inc.

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

/* Required to keep collect2.c happy */
#undef OBJECT_FORMAT_COFF 

#define HAVE_ATEXIT

/* If we don't set MASK_ABICALLS, we can't default to PIC. */
#undef TARGET_DEFAULT
#define TARGET_DEFAULT (MASK_ABICALLS|MASK_GAS)


/* Provide a STARTFILE_SPEC appropriate for GNU/Linux.  Here we add
   the GNU/Linux magical crtbegin.o file (see crtstuff.c) which
   provides part of the support for getting C++ file-scope static
   object constructed before entering `main'. */

#undef  STARTFILE_SPEC
#define STARTFILE_SPEC \
  "%{!shared: \
     %{pg:gcrt1.o%s} %{!pg:%{p:gcrt1.o%s} %{!p:crt1.o%s}}}\
   crti.o%s %{!shared:crtbegin.o%s} %{shared:crtbeginS.o%s}"

/* Provide a ENDFILE_SPEC appropriate for GNU/Linux.  Here we tack on
   the GNU/Linux magical crtend.o file (see crtstuff.c) which
   provides part of the support for getting C++ file-scope static
   object constructed before entering `main', followed by a normal
   GNU/Linux "finalizer" file, `crtn.o'.  */

#undef  ENDFILE_SPEC
#define ENDFILE_SPEC \
  "%{!shared:crtend.o%s} %{shared:crtendS.o%s} crtn.o%s"

/* From iris5.h */
/* -G is incompatible with -KPIC which is the default, so only allow objects
   in the small data section if the user explicitly asks for it.  */
#undef MIPS_DEFAULT_GVALUE
#define MIPS_DEFAULT_GVALUE 0

/* Borrowed from sparc/linux.h */
#undef LINK_SPEC
#define LINK_SPEC "-Y P,/usr/lib %{shared:-shared} \
  %{!shared: \
    %{!ibcs: \
      %{!static: \
        %{rdynamic:-export-dynamic} \
        %{!dynamic-linker:-dynamic-linker /lib/ld.so.1}} \
        %{static:-static}}}"


#undef SUBTARGET_ASM_SPEC
#define SUBTARGET_ASM_SPEC "-KPIC"

/* Undefine the following which were defined in elf.h.  This will cause the linux
   port to continue to use collect2 for constructors/destructors.  These may be removed
   when .ctor/.dtor section support is desired. */

#undef CTORS_SECTION_ASM_OP
#undef DTORS_SECTION_ASM_OP

#undef EXTRA_SECTIONS
#define EXTRA_SECTIONS in_sdata, in_rdata

#undef INVOKE__main
#undef NAME__MAIN
#undef SYMBOL__MAIN

#undef EXTRA_SECTION_FUNCTIONS
#define EXTRA_SECTION_FUNCTIONS                                         \
  SECTION_FUNCTION_TEMPLATE(sdata_section, in_sdata, SDATA_SECTION_ASM_OP) \
  SECTION_FUNCTION_TEMPLATE(rdata_section, in_rdata, RDATA_SECTION_ASM_OP)

#undef ASM_OUTPUT_CONSTRUCTOR
#undef ASM_OUTPUT_DESTRUCTOR

#undef CTOR_LIST_BEGIN
#undef CTOR_LIST_END
#undef DTOR_LIST_BEGIN
#undef DTOR_LIST_END

/*  End of undefines to turn off .ctor/.dtor section support */
