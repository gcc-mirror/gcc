/* Definitions for SPARC running Linux-based GNU systems with ELF.
   Copyright (C) 1996, 1997, 1998, 1999, 2000, 2002, 2003, 2004
   Free Software Foundation, Inc.
   Contributed by Eddie C. Dost (ecd@skynet.be)

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#define TARGET_OS_CPP_BUILTINS()		\
  do						\
    {						\
	builtin_define_std ("unix");		\
	builtin_define_std ("linux");		\
	builtin_define ("__gnu_linux__");	\
	builtin_assert ("system=linux");	\
	builtin_assert ("system=unix");		\
	builtin_assert ("system=posix");	\
    }						\
  while (0)

/* Don't assume anything about the header files.  */
#define NO_IMPLICIT_EXTERN_C

#undef MD_EXEC_PREFIX
#undef MD_STARTFILE_PREFIX

/* Provide a STARTFILE_SPEC appropriate for GNU/Linux.  Here we add
   the GNU/Linux magical crtbegin.o file (see crtstuff.c) which
   provides part of the support for getting C++ file-scope static
   object constructed before entering `main'.  */
   
#undef  STARTFILE_SPEC
#ifdef USE_GNULIBC_1
#define STARTFILE_SPEC \
  "%{!shared: \
     %{pg:gcrt1.o%s} %{!pg:%{p:gcrt1.o%s} %{!p:crt1.o%s}}}\
   crti.o%s %{!shared:crtbegin.o%s} %{shared:crtbeginS.o%s}"
#elif defined HAVE_LD_PIE
#define STARTFILE_SPEC \
  "%{!shared: %{pg|p:gcrt1.o%s;pie:Scrt1.o%s;:crt1.o%s}}\
   crti.o%s %{static:crtbeginT.o%s;shared|pie:crtbeginS.o%s;:crtbegin.o%s}"
#else
#define STARTFILE_SPEC \
  "%{!shared: %{pg|p:gcrt1.o%s;:crt1.o%s}}\
   crti.o%s %{static:crtbeginT.o%s;shared|pie:crtbeginS.o%s;:crtbegin.o%s}"
#endif

/* Provide a ENDFILE_SPEC appropriate for GNU/Linux.  Here we tack on
   the GNU/Linux magical crtend.o file (see crtstuff.c) which
   provides part of the support for getting C++ file-scope static
   object constructed before entering `main', followed by a normal
   GNU/Linux "finalizer" file, `crtn.o'.  */

#undef  ENDFILE_SPEC
#define ENDFILE_SPEC \
  "%{ffast-math|funsafe-math-optimizations:crtfastmath.o%s} \
   %{shared|pie:crtendS.o%s;:crtend.o%s} crtn.o%s"

/* This is for -profile to use -lc_p instead of -lc.  */
#undef	CC1_SPEC
#define	CC1_SPEC "%{profile:-p} \
%{sun4:} %{target:} \
%{mcypress:-mcpu=cypress} \
%{msparclite:-mcpu=sparclite} %{mf930:-mcpu=f930} %{mf934:-mcpu=f934} \
%{mv8:-mcpu=v8} %{msupersparc:-mcpu=supersparc} \
"

/* The GNU C++ standard library requires that these macros be defined.  */
#undef CPLUSPLUS_CPP_SPEC
#define CPLUSPLUS_CPP_SPEC "-D_GNU_SOURCE %(cpp)"

#undef TARGET_VERSION
#define TARGET_VERSION fprintf (stderr, " (sparc GNU/Linux with ELF)");

#undef SUBTARGET_SWITCHES
#define SUBTARGET_SWITCHES \
{"long-double-64", -MASK_LONG_DOUBLE_128, N_("Use 64 bit long doubles") }, \
{"long-double-128", MASK_LONG_DOUBLE_128, N_("Use 128 bit long doubles") },

#undef SIZE_TYPE
#define SIZE_TYPE "unsigned int"
 
#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE "int"
  
#undef WCHAR_TYPE
#define WCHAR_TYPE "int"
   
#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE 32

#undef CPP_SUBTARGET_SPEC
#ifdef USE_GNULIBC_1
#define CPP_SUBTARGET_SPEC \
"%{fPIC|fPIE|fpic|fpie:-D__PIC__ -D__pic__} %{posix:-D_POSIX_SOURCE} \
%{mlong-double-128:-D__LONG_DOUBLE_128__}"
#else
#define CPP_SUBTARGET_SPEC \
"%{fPIC|fPIE|fpic|fpie:-D__PIC__ -D__pic__} %{posix:-D_POSIX_SOURCE} \
%{pthread:-D_REENTRANT} %{mlong-double-128:-D__LONG_DOUBLE_128__}"
#endif

#undef LIB_SPEC
/* We no longer link with libc_p.a or libg.a by default. If you
   want to profile or debug the GNU/Linux C library, please add
   -lc_p or -ggdb to LDFLAGS at the link time, respectively.  */
#if 1
#ifdef USE_GNULIBC_1
#define LIB_SPEC \
  "%{!shared: %{p:-lgmon} %{pg:-lgmon} %{profile:-lgmon -lc_p} \
     %{!profile:%{!ggdb:-lc} %{ggdb:-lg}}}"
#else
#define LIB_SPEC \
  "%{pthread:-lpthread} \
   %{shared:-lc} \
   %{!shared:%{mieee-fp:-lieee} %{profile:-lc_p}%{!profile:-lc}}"
#endif
#else
#define LIB_SPEC \
  "%{!shared: \
     %{mieee-fp:-lieee} %{p:-lgmon -lc_p} %{pg:-lgmon -lc_p} \
       %{!p:%{!pg:%{!g*:-lc} %{g*:-lg}}}}"
#endif

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

/* If ELF is the default format, we should not use /lib/elf.  */

#undef  LINK_SPEC
#ifdef USE_GNULIBC_1
#define LINK_SPEC "-m elf32_sparc -Y P,/usr/lib %{shared:-shared} \
  %{!shared: \
    %{!ibcs: \
      %{!static: \
        %{rdynamic:-export-dynamic} \
        %{!dynamic-linker:-dynamic-linker /lib/ld-linux.so.1}} \
        %{static:-static}}}"
#else
#define LINK_SPEC "-m elf32_sparc -Y P,/usr/lib %{shared:-shared} \
  %{!mno-relax:%{!r:-relax}} \
  %{!shared: \
    %{!ibcs: \
      %{!static: \
        %{rdynamic:-export-dynamic} \
        %{!dynamic-linker:-dynamic-linker /lib/ld-linux.so.2}} \
        %{static:-static}}}"
#endif

/* The sun bundled assembler doesn't accept -Yd, (and neither does gas).
   It's safe to pass -s always, even if -g is not used.  */
#undef ASM_SPEC
#define ASM_SPEC \
  "%{V} %{v:%{!V:-V}} %{!Qn:-Qy} %{n} %{T} %{Ym,*} %{Wa,*:%*} -s \
   %{fpic|fPIC|fpie|fPIE:-K PIC} %(asm_cpu) %(asm_relax)"

/* Same as sparc.h */
#undef DBX_REGISTER_NUMBER
#define DBX_REGISTER_NUMBER(REGNO) (REGNO)

#undef ASM_OUTPUT_ALIGNED_LOCAL
#define ASM_OUTPUT_ALIGNED_LOCAL(FILE, NAME, SIZE, ALIGN)		\
do {									\
  fputs ("\t.local\t", (FILE));		\
  assemble_name ((FILE), (NAME));					\
  putc ('\n', (FILE));							\
  ASM_OUTPUT_ALIGNED_COMMON (FILE, NAME, SIZE, ALIGN);			\
} while (0)

#undef COMMON_ASM_OP
#define COMMON_ASM_OP "\t.common\t"

#undef  LOCAL_LABEL_PREFIX
#define LOCAL_LABEL_PREFIX  "."

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
  sprintf (LABEL, "*.L%s%ld", PREFIX, (long)(NUM))


/* Define for support of TFmode long double.
   SPARC ABI says that long double is 4 words.  */
#define LONG_DOUBLE_TYPE_SIZE (TARGET_LONG_DOUBLE_128 ? 128 : 64)

/* Constant which presents upper bound of the above value.  */
#define MAX_LONG_DOUBLE_TYPE_SIZE 128

/* Define this to set long double type size to use in libgcc2.c, which can
   not depend on target_flags.  */
#ifdef __LONG_DOUBLE_128__
#define LIBGCC2_LONG_DOUBLE_TYPE_SIZE 128
#else
#define LIBGCC2_LONG_DOUBLE_TYPE_SIZE 64
#endif

#undef DITF_CONVERSION_LIBFUNCS
#define DITF_CONVERSION_LIBFUNCS 1

#if !defined(USE_GNULIBC_1) && defined(HAVE_LD_EH_FRAME_HDR)
#define LINK_EH_SPEC "%{!static:--eh-frame-hdr} "
#endif

#ifdef HAVE_AS_TLS
#undef TARGET_SUN_TLS
#undef TARGET_GNU_TLS
#define TARGET_SUN_TLS 0
#define TARGET_GNU_TLS 1
#endif

/* Don't be different from other Linux platforms in this regard.  */
#define HANDLE_PRAGMA_PACK_PUSH_POP

/* We use GNU ld so undefine this so that attribute((init_priority)) works.  */
#undef CTORS_SECTION_ASM_OP
#undef DTORS_SECTION_ASM_OP

#define TARGET_ASM_FILE_END file_end_indicate_exec_stack

/* Determine whether the the entire c99 runtime is present in the
   runtime library.  */
#define TARGET_C99_FUNCTIONS 1

#define TARGET_HAS_F_SETLKW

#undef LINK_GCC_C_SEQUENCE_SPEC
#define LINK_GCC_C_SEQUENCE_SPEC \
  "%{static:--start-group} %G %L %{static:--end-group}%{!static:%G}"

/* Do code reading to identify a signal frame, and set the frame
   state data appropriately.  See unwind-dw2.c for the structs.  */

#define MD_FALLBACK_FRAME_STATE_FOR(CONTEXT, FS, SUCCESS)		\
  do {									\
    unsigned int *pc_ = (CONTEXT)->ra;					\
    int new_cfa_, i_, oldstyle_;					\
    int regs_off_, fpu_save_off_;					\
    int fpu_save_, this_cfa_;						\
									\
    if (pc_[1] != 0x91d02010)		/* ta 0x10 */			\
      break;								\
    if (pc_[0] == 0x821020d8)		/* mov NR_sigreturn, %g1 */	\
      oldstyle_ = 1;							\
    else if (pc_[0] == 0x82102065)	/* mov NR_rt_sigreturn, %g1 */	\
      oldstyle_ = 0;							\
    else								\
      break;								\
    if (oldstyle_)							\
      {									\
        regs_off_ = 96;							\
        fpu_save_off_ = regs_off_ + (4 * 4) + (16 * 4);			\
      }									\
    else								\
      {									\
        regs_off_ = 96 + 128;						\
        fpu_save_off_ = regs_off_ + (4 * 4) + (16 * 4) + (2 * 4);	\
      }									\
    this_cfa_ = (int) (CONTEXT)->cfa;					\
    new_cfa_ = *(int *)(((CONTEXT)->cfa) + (regs_off_+(4*4)+(14 * 4)));	\
    fpu_save_ = *(int *)((this_cfa_) + (fpu_save_off_));		\
    (FS)->cfa_how = CFA_REG_OFFSET;					\
    (FS)->cfa_reg = 14;							\
    (FS)->cfa_offset = new_cfa_ - (int) (CONTEXT)->cfa;			\
    for (i_ = 1; i_ < 16; ++i_)						\
      {									\
        if (i_ == 14)							\
          continue;							\
	(FS)->regs.reg[i_].how = REG_SAVED_OFFSET;			\
	(FS)->regs.reg[i_].loc.offset =					\
	   this_cfa_ + (regs_off_+(4 * 4)+(i_ * 4)) - new_cfa_;		\
      }									\
    for (i_ = 0; i_ < 16; ++i_)						\
      {									\
	(FS)->regs.reg[i_ + 16].how = REG_SAVED_OFFSET;			\
	(FS)->regs.reg[i_ + 16].loc.offset =				\
	  this_cfa_ + (i_ * 4) - new_cfa_;				\
      }									\
    if (fpu_save_)							\
      {									\
	for (i_ = 0; i_ < 32; ++i_)					\
	  {								\
	    (FS)->regs.reg[i_ + 32].how = REG_SAVED_OFFSET;		\
	    (FS)->regs.reg[i_ + 32].loc.offset =			\
	      (fpu_save_ + (i_ * 4)) - new_cfa_;			\
	  }								\
      }									\
    /* Stick return address into %g0, same trick Alpha uses.  */	\
    (FS)->regs.reg[0].how = REG_SAVED_OFFSET;				\
    (FS)->regs.reg[0].loc.offset = this_cfa_+(regs_off_+4)-new_cfa_;	\
    (FS)->retaddr_column = 0;						\
    goto SUCCESS;							\
  } while (0)
