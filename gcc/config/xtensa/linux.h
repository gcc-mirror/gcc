/* Xtensa Linux configuration.
   Derived from the configuration for GCC for Intel i386 running Linux.
   Copyright (C) 2001, 2002 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

#undef TARGET_VERSION
#define TARGET_VERSION fputs (" (Xtensa GNU/Linux with ELF)", stderr);

#undef ASM_SPEC
#define ASM_SPEC "%{v} %{mno-density:--no-density} \
                  %{mtext-section-literals:--text-section-literals} \
                  %{mno-text-section-literals:--no-text-section-literals} \
		  %{mtarget-align:--target-align} \
		  %{mno-target-align:--no-target-align} \
		  %{mlongcalls:--longcalls} \
		  %{mno-longcalls:--no-longcalls}"

#undef ASM_FINAL_SPEC

#undef LIB_SPEC
#define LIB_SPEC \
  "%{shared: -lc} \
   %{!shared: %{pthread:-lpthread} \
   %{profile:-lc_p} %{!profile: -lc}}"

#undef LINK_SPEC
#define LINK_SPEC \
 "%{shared:-shared} \
  %{!shared: \
    %{!ibcs: \
      %{!static: \
        %{rdynamic:-export-dynamic} \
        %{!dynamic-linker:-dynamic-linker /lib/ld.so.1}} \
      %{static:-static}}}"

#undef CPP_PREDEFINES
#define CPP_PREDEFINES \
 "-D__XTENSA__ -D__ELF__ -Acpu=xtensa -Amachine=xtensa \
  -Dunix -D__gnu_linux__ -Dlinux -Asystem=posix"

#undef LOCAL_LABEL_PREFIX
#define LOCAL_LABEL_PREFIX	"."

/* Don't switch sections in the middle of a literal pool! */
#undef SELECT_RTX_SECTION
#define SELECT_RTX_SECTION(MODE,RTX,ALIGN)

/* Always enable "-fpic" for Xtensa Linux.  */
#define XTENSA_ALWAYS_PIC 1

/* Redefine the standard ELF version of ASM_DECLARE_FUNCTION_SIZE to
   allow adding the ".end literal_prefix" directive at the end of the
   function.  */
#undef ASM_DECLARE_FUNCTION_SIZE
#define ASM_DECLARE_FUNCTION_SIZE(FILE, FNAME, DECL)		\
  do								\
    {								\
      if (!flag_inhibit_size_directive)				\
	{							\
	  char label[256];					\
	  static int labelno;					\
	  							\
	  labelno++;						\
	  							\
	  ASM_GENERATE_INTERNAL_LABEL (label, "Lfe", labelno);	\
	  ASM_OUTPUT_INTERNAL_LABEL (FILE, "Lfe", labelno);	\
	  							\
	  fprintf (FILE, "%s", SIZE_ASM_OP);			\
	  assemble_name (FILE, (FNAME));			\
	  fprintf (FILE, ",");					\
	  assemble_name (FILE, label);				\
	  fprintf (FILE, "-");					\
	  assemble_name (FILE, (FNAME));			\
	  putc ('\n', FILE);					\
	}							\
      XTENSA_DECLARE_FUNCTION_SIZE(FILE, FNAME, DECL);		\
    }								\
  while (0)
