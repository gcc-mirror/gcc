/* Definitions for 64-bit SPARC running Linux-based GNU systems with ELF.
   Copyright 1996, 1997, 1998 Free Software Foundation, Inc.
   Contributed by David S. Miller (davem@caip.rutgers.edu)

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

#define SPARC_BI_ARCH

#define LINUX_DEFAULT_ELF

/* Don't assume anything about the header files. */
#define NO_IMPLICIT_EXTERN_C

#undef DEFAULT_VTABLE_THUNKS
#define DEFAULT_VTABLE_THUNKS 1

#include <sparc/sysv4.h>

#undef MD_EXEC_PREFIX
#undef MD_STARTFILE_PREFIX

#if TARGET_CPU_DEFAULT == TARGET_CPU_v9 || TARGET_CPU_DEFAULT == TARGET_CPU_ultrasparc
/* A 64 bit v9 compiler with stack-bias,
   in a Medium/Low code model environment.  */

#undef TARGET_DEFAULT
#define TARGET_DEFAULT \
  (MASK_V9 + MASK_PTR64 + MASK_64BIT /* + MASK_HARD_QUAD */ \
   + MASK_STACK_BIAS + MASK_APP_REGS + MASK_EPILOGUE + MASK_FPU)
#endif

/* Output at beginning of assembler file.  */
/* The .file command should always begin the output.  */
#undef ASM_FILE_START
#define ASM_FILE_START(FILE)                                            \
  do {                                                                  \
        output_file_directive (FILE, main_input_filename);              \
        fprintf (FILE, "\t.version\t\"01.01\"\n");                      \
  } while (0)
  
#undef ASM_CPU_DEFAULT_SPEC
#define ASM_CPU_DEFAULT_SPEC "-Av9a"

/* Provide a STARTFILE_SPEC appropriate for GNU/Linux.  Here we add
   the GNU/Linux magical crtbegin.o file (see crtstuff.c) which
   provides part of the support for getting C++ file-scope static
   object constructed before entering `main'. */
   
#undef  STARTFILE_SPEC

#define STARTFILE_SPEC32 \
  "%{!shared: \
     %{pg:gcrt1.o%s} %{!pg:%{p:gcrt1.o%s} %{!p:crt1.o%s}}}\
   crti.o%s %{!shared:crtbegin.o%s} %{shared:crtbeginS.o%s}"

#define STARTFILE_SPEC64 \
  "%{!shared: \
     %{pg:/usr/lib64/gcrt1.o%s} %{!pg:%{p:/usr/lib64/gcrt1.o%s} %{!p:/usr/lib64/crt1.o%s}}}\
   /usr/lib64/crti.o%s %{!shared:crtbegin.o%s} %{shared:crtbeginS.o%s}"
   
#ifdef SPARC_BI_ARCH

#if DEFAULT_ARCH32_P
#define STARTFILE_SPEC "\
%{m32:" STARTFILE_SPEC32 "} \
%{m64:" STARTFILE_SPEC64 "} \
%{!m32:%{!m64:" STARTFILE_SPEC32 "}}"
#else
#define STARTFILE_SPEC "\
%{m32:" STARTFILE_SPEC32 "} \
%{m64:" STARTFILE_SPEC64 "} \
%{!m32:%{!m64:" STARTFILE_SPEC64 "}}"
#endif

#else

#define STARTFILE_SPEC STARTFILE_SPEC64

#endif

/* Provide a ENDFILE_SPEC appropriate for GNU/Linux.  Here we tack on
   the GNU/Linux magical crtend.o file (see crtstuff.c) which
   provides part of the support for getting C++ file-scope static
   object constructed before entering `main', followed by a normal
   GNU/Linux "finalizer" file, `crtn.o'.  */

#undef  ENDFILE_SPEC

#define ENDFILE_SPEC32 \
  "%{!shared:crtend.o%s} %{shared:crtendS.o%s} crtn.o%s"

#define ENDFILE_SPEC64 \
  "%{!shared:crtend.o%s} %{shared:crtendS.o%s} /usr/lib64/crtn.o%s"
  
#ifdef SPARC_BI_ARCH

#if DEFAULT_ARCH32_P
#define ENDFILE_SPEC "\
%{m32:" ENDFILE_SPEC32 "} \
%{m64:" ENDFILE_SPEC64 "} \
%{!m32:%{!m64:" ENDFILE_SPEC32 "}}"
#else
#define ENDFILE_SPEC "\
%{m32:" ENDFILE_SPEC32 "} \
%{m64:" ENDFILE_SPEC64 "} \
%{!m32:%{!m64:" ENDFILE_SPEC64 "}}"
#endif

#else

#define ENDFILE_SPEC ENDFILE_SPEC64

#endif

#undef TARGET_VERSION
#define TARGET_VERSION fprintf (stderr, " (sparc64 GNU/Linux with ELF)");

/* The default code model.  */
#undef SPARC_DEFAULT_CMODEL
#define SPARC_DEFAULT_CMODEL CM_MEDLOW

#undef WCHAR_TYPE
#define WCHAR_TYPE "int"

#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE 32

#undef MAX_WCHAR_TYPE_SIZE

#undef LONG_DOUBLE_TYPE_SIZE
#define LONG_DOUBLE_TYPE_SIZE 128

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-D__ELF__ -Dunix -D_LONGLONG -D__sparc__ -Dlinux -Asystem(unix) -Asystem(posix)"

#undef CPP_SUBTARGET_SPEC
#define CPP_SUBTARGET_SPEC "\
%{fPIC:-D__PIC__ -D__pic__} \
%{fpic:-D__PIC__ -D__pic__} \
%{posix:-D_POSIX_SOURCE} \
%{pthread:-D_REENTRANT} \
"

#undef LIB_SPEC
#define LIB_SPEC \
  "%{shared: -lc} \
   %{!shared: %{mieee-fp:-lieee} %{pthread:-lpthread} \
     %{profile:-lc_p} %{!profile: -lc}}"

/* Provide a LINK_SPEC appropriate for GNU/Linux.  Here we provide support
   for the special GCC options -static and -shared, which allow us to
   link things in one of these three modes by applying the appropriate
   combinations of options at link-time. We like to support here for
   as many of the other GNU linker options as possible. But I don't
   have the time to search for those flags. I am sure how to add
   support for -soname shared_object_name. H.J.

   I took out %{v:%{!V:-V}}. It is too much :-(. They can use
   -Wl,-V.

   When the -shared link option is used a final link is not being
   done.  */

/* If ELF is the default format, we should not use /lib/elf. */

#ifdef SPARC_BI_ARCH

#undef SUBTARGET_EXTRA_SPECS
#define SUBTARGET_EXTRA_SPECS \
  { "link_arch32",       LINK_ARCH32_SPEC },              \
  { "link_arch64",       LINK_ARCH64_SPEC },              \
  { "link_arch_default", LINK_ARCH_DEFAULT_SPEC },	  \
  { "link_arch",	 LINK_ARCH_SPEC },
    
#define LINK_ARCH32_SPEC "-m elf32_sparc -Y P,/usr/lib %{shared:-shared} \
  %{!shared: \
    %{!ibcs: \
      %{!static: \
        %{rdynamic:-export-dynamic} \
        %{!dynamic-linker:-dynamic-linker /lib/ld-linux.so.2}} \
        %{static:-static}}} \
"

#define LINK_ARCH64_SPEC "-m elf64_sparc -Y P,/usr/lib64 %{shared:-shared} \
  %{!shared: \
    %{!ibcs: \
      %{!static: \
        %{rdynamic:-export-dynamic} \
        %{!dynamic-linker:-dynamic-linker /lib64/ld-linux.so.2}} \
        %{static:-static}}} \
"

#define LINK_ARCH_SPEC "\
%{m32:%(link_arch32)} \
%{m64:%(link_arch64)} \
%{!m32:%{!m64:%(link_arch_default)}} \
"

#define LINK_ARCH_DEFAULT_SPEC \
(DEFAULT_ARCH32_P ? LINK_ARCH32_SPEC : LINK_ARCH64_SPEC)

#undef  LINK_SPEC
#define LINK_SPEC "\
%(link_arch) \
%{mlittle-endian:-EL} \
"

#undef	CC1_SPEC
#if DEFAULT_ARCH32_P
#define CC1_SPEC "\
%{sun4:} %{target:} \
%{mcypress:-mcpu=cypress} \
%{msparclite:-mcpu=sparclite} %{mf930:-mcpu=f930} %{mf934:-mcpu=f934} \
%{mv8:-mcpu=v8} %{msupersparc:-mcpu=supersparc} \
%{m64:-mptr64 -mstack-bias \
  %{!mcpu*:%{!mcypress:%{!msparclite:%{!mf930:%{!mf934:%{!mv8:%{!msupersparc:-mcpu=ultrasparc}}}}}}} \
  %{!mno-vis:%{!mcpu=v9:-mvis}}} \
%{!m64:%{g*:%{!gs*:%{!gd*:%{!gx*:%{!gc*:-gstabs+}}}}}} \
"
#else
#define CC1_SPEC "\
%{sun4:} %{target:} \
%{mcypress:-mcpu=cypress} \
%{msparclite:-mcpu=sparclite} %{mf930:-mcpu=f930} %{mf934:-mcpu=f934} \
%{mv8:-mcpu=v8} %{msupersparc:-mcpu=supersparc} \
%{m32:-mptr32 -mno-stack-bias \
  %{!mcpu*:%{!mcypress:%{!msparclite:%{!mf930:%{!mf934:%{!mv8:%{!msupersparc:-mcpu=cypress}}}}}}} \
  %{g*:%{!gs*:%{!gd*:%{!gx*:%{!gc*:-gstabs+}}}}}} \
%{!m32:%{!mcpu*:-mcpu=ultrasparc}} \
%{!mno-vis:%{!m32:%{!mcpu=v9:-mvis}}} \
"
#endif

#if DEFAULT_ARCH32_P
#define MULTILIB_DEFAULTS { "m32" }
#else
#define MULTILIB_DEFAULTS { "m64" }
#endif

#else /* !SPARC_BI_ARCH */

#undef LINK_SPEC
#define LINK_ARCH_SPEC "-m elf64_sparc -Y P,/usr/lib64 %{shared:-shared} \
  %{!shared: \
    %{!ibcs: \
      %{!static: \
        %{rdynamic:-export-dynamic} \
        %{!dynamic-linker:-dynamic-linker /lib64/ld-linux.so.2}} \
        %{static:-static}}} \
%{mlittle-endian:-EL} \
"

#endif /* !SPARC_BI_ARCH */

/* The sun bundled assembler doesn't accept -Yd, (and neither does gas).
   It's safe to pass -s always, even if -g is not used. */
#undef ASM_SPEC
#define ASM_SPEC "\
%{V} \
%{v:%{!V:-V}} \
%{!Qn:-Qy} \
%{n} \
%{T} \
%{Ym,*} \
%{Wa,*:%*} \
-s %{fpic:-K PIC} %{fPIC:-K PIC} \
%{mlittle-endian:-EL} \
%(asm_cpu) %(asm_arch) \
"

/* Same as sparc.h */
#undef DBX_REGISTER_NUMBER
#define DBX_REGISTER_NUMBER(REGNO) (REGNO)

/* System V Release 4 uses DWARF debugging info.  Buf DWARF1 doesn't do
   64-bit anything, so we use DWARF2.  */

#undef DWARF2_DEBUGGING_INFO
#undef DWARF_DEBUGGING_INFO
#undef DBX_DEBUGGING_INFO
#define DWARF2_DEBUGGING_INFO
#define DBX_DEBUGGING_INFO

#undef PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE DWARF2_DEBUG

#undef ASM_OUTPUT_ALIGNED_LOCAL
#define ASM_OUTPUT_ALIGNED_LOCAL(FILE, NAME, SIZE, ALIGN)		\
do {									\
  fputs ("\t.local\t", (FILE));		\
  assemble_name ((FILE), (NAME));					\
  putc ('\n', (FILE));							\
  ASM_OUTPUT_ALIGNED_COMMON (FILE, NAME, SIZE, ALIGN);			\
} while (0)

#undef COMMON_ASM_OP
#define COMMON_ASM_OP "\t.common"

/* This is how to output a definition of an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.  */

#undef  ASM_OUTPUT_INTERNAL_LABEL
#define ASM_OUTPUT_INTERNAL_LABEL(FILE,PREFIX,NUM)	\
  fprintf (FILE, ".L%s%d:\n", PREFIX, NUM)

/* This is how to output a reference to an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.  */

#undef  ASM_OUTPUT_INTERNAL_LABELREF
#define ASM_OUTPUT_INTERNAL_LABELREF(FILE,PREFIX,NUM)	\
  fprintf (FILE, ".L%s%d", PREFIX, NUM)

/* This is how to store into the string LABEL
   the symbol_ref name of an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.
   This is suitable for output with `assemble_name'.  */

#undef  ASM_GENERATE_INTERNAL_LABEL
#define ASM_GENERATE_INTERNAL_LABEL(LABEL,PREFIX,NUM)	\
  sprintf (LABEL, "*.L%s%d", PREFIX, NUM)

/* Define the names of various pseudo-ops used by the Sparc/svr4 assembler.
   ??? If ints are 64 bits then UNALIGNED_INT_ASM_OP (defined elsewhere) is
   misnamed.  These should all refer to explicit sizes (half/word/xword?),
   anything other than short/int/long/etc.  */

#define UNALIGNED_DOUBLE_INT_ASM_OP	".uaxword"

/* DWARF bits.  */

/* Follow Irix 6 and not the Dwarf2 draft in using 64-bit offsets. 
   Obviously the Dwarf2 folks havn't tried to actually build systems
   with their spec.  On a 64-bit system, only 64-bit relocs become
   RELATIVE relocations.  */

/* #define DWARF_OFFSET_SIZE PTR_SIZE */

/* No weird SPARC variants on Linux */
#undef TARGET_LIVE_G0
#define TARGET_LIVE_G0			0
#undef TARGET_BROKEN_SAVERESTORE
#define TARGET_BROKEN_SAVERESTORE	0

#if TARGET_ARCH32
/* Override MACHINE_STATE_{SAVE,RESTORE} because we have special
   traps available which can get and set the condition codes
   reliably.  */
#undef MACHINE_STATE_SAVE
#define MACHINE_STATE_SAVE(ID)				\
  unsigned long int ms_flags, ms_saveret;		\
  asm volatile("ta	0x20\n\t"			\
	       "mov	%%g1, %0\n\t"			\
	       "mov	%%g2, %1\n\t"			\
	       : "=r" (ms_flags), "=r" (ms_saveret));

#undef MACHINE_STATE_RESTORE
#define MACHINE_STATE_RESTORE(ID)			\
  asm volatile("mov	%0, %%g1\n\t"			\
	       "mov	%1, %%g2\n\t"			\
	       "ta	0x21\n\t"			\
	       : /* no outputs */			\
	       : "r" (ms_flags), "r" (ms_saveret));
#endif /* sparc32 */

/* A C statement (sans semicolon) to output an element in the table of
   global constructors.  */
#undef ASM_OUTPUT_CONSTRUCTOR
#define ASM_OUTPUT_CONSTRUCTOR(FILE,NAME)				\
  do {									\
    ctors_section ();							\
    fprintf (FILE, "\t%s\t ", TARGET_ARCH64 ? ASM_LONGLONG : INT_ASM_OP); \
    assemble_name (FILE, NAME);						\
    fprintf (FILE, "\n");						\
  } while (0)

/* A C statement (sans semicolon) to output an element in the table of
   global destructors.  */
#undef ASM_OUTPUT_DESTRUCTOR
#define ASM_OUTPUT_DESTRUCTOR(FILE,NAME)       				\
  do {									\
    dtors_section ();                   				\
    fprintf (FILE, "\t%s\t ", TARGET_ARCH64 ? ASM_LONGLONG : INT_ASM_OP); \
    assemble_name (FILE, NAME);              				\
    fprintf (FILE, "\n");						\
  } while (0)
