/* Definitions for SH running Linux-based GNU systems using ELF
   Copyright (C) 1999, 2000, 2002, 2003, 2004, 2005
   Free Software Foundation, Inc.
   Contributed by Kazumoto Kojima <kkojima@rr.iij4u.or.jp>

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
along with this program; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* Run-time Target Specification.  */
#undef TARGET_VERSION
#define TARGET_VERSION  fputs (" (SH GNU/Linux with ELF)", stderr);

/* We're not SYSVR4, not having /usr/ccs */
#undef MD_EXEC_PREFIX
#undef MD_STARTFILE_PREFIX

/* This was defined in linux.h.  Define it here also.  */
#define HANDLE_PRAGMA_PACK_PUSH_POP

/* Don't assume anything about the header files.  */
#define NO_IMPLICIT_EXTERN_C

/* The GNU C++ standard library requires that these macros be defined.  */
#undef CPLUSPLUS_CPP_SPEC
#define CPLUSPLUS_CPP_SPEC "-D_GNU_SOURCE %(cpp)"

/* Enable DWARF 2 exceptions.  */
#undef DWARF2_UNWIND_INFO
#define DWARF2_UNWIND_INFO 1

#undef SUBTARGET_CPP_SPEC
#define SUBTARGET_CPP_SPEC "\
   %{posix:-D_POSIX_SOURCE} \
   %{pthread:-D_REENTRANT -D_PTHREADS} \
"

#define TARGET_OS_CPP_BUILTINS() \
do { \
  builtin_define_std ("unix"); \
  builtin_define ("__gnu_linux__"); \
  builtin_define_std ("linux"); \
  builtin_assert ("system=linux"); \
  builtin_assert ("system=unix"); \
  builtin_assert ("system=posix"); \
} while (0)

#undef TARGET_DEFAULT
#define TARGET_DEFAULT \
  (TARGET_CPU_DEFAULT | USERMODE_BIT | TARGET_ENDIAN_DEFAULT)

#define TARGET_ASM_FILE_END file_end_indicate_exec_stack

#undef SUBTARGET_LINK_EMUL_SUFFIX
#define SUBTARGET_LINK_EMUL_SUFFIX "_linux"
#undef SUBTARGET_LINK_SPEC
#define SUBTARGET_LINK_SPEC \
  "%{shared:-shared} \
   %{!static: \
     %{rdynamic:-export-dynamic} \
     %{!dynamic-linker:-dynamic-linker /lib/ld-linux.so.2}} \
   %{static:-static}"

#undef LIB_SPEC
#define LIB_SPEC \
  "%{pthread:-lpthread} \
   %{shared: -lc} \
   %{!shared: \
     %{mieee-fp:-lieee} \
     %{profile:-lc_p} %{!profile: -lc}}"

#if defined(HAVE_LD_EH_FRAME_HDR)
#undef LINK_EH_SPEC
#define LINK_EH_SPEC "%{!static:--eh-frame-hdr} "
#endif

#undef STARTFILE_SPEC
#if defined HAVE_LD_PIE
#define STARTFILE_SPEC \
  "%{!shared: %{pg|p|profile:gcrt1.o%s;pie:Scrt1.o%s;:crt1.o%s}} \
   crti.o%s %{static:crtbeginT.o%s;shared|pie:crtbeginS.o%s;:crtbegin.o%s}"
#else
#define STARTFILE_SPEC \
  "%{!shared: %{pg|p|profile:gcrt1.o%s;:crt1.o%s}} \
   crti.o%s %{static:crtbeginT.o%s;shared|pie:crtbeginS.o%s;:crtbegin.o%s}"
#endif

#undef	ENDFILE_SPEC
#define ENDFILE_SPEC \
  "%{shared|pie:crtendS.o%s;:crtend.o%s} crtn.o%s"

#define LINK_GCC_C_SEQUENCE_SPEC \
  "%{static:--start-group} %G %L %{static:--end-group}%{!static:%G}"

/* Use --as-needed -lgcc_s for eh support.  */
#ifdef HAVE_LD_AS_NEEDED
#define USE_LD_AS_NEEDED 1
#endif

/* Determine whether the the entire c99 runtime
   is present in the runtime library.  */
#define TARGET_C99_FUNCTIONS 1

/* Output assembler code to STREAM to call the profiler.  */

#undef FUNCTION_PROFILER
#define FUNCTION_PROFILER(STREAM,LABELNO)				\
  do {									\
    if (flag_pic)							\
      {									\
	fprintf (STREAM, "\tmov.l\t3f,r1\n");				\
	fprintf (STREAM, "\tmova\t3f,r0\n");				\
	fprintf (STREAM, "\tadd\tr1,r0\n");				\
	fprintf (STREAM, "\tmov.l\t1f,r1\n");				\
	fprintf (STREAM, "\tmov.l\t@(r0,r1),r1\n");			\
      }									\
    else								\
      fprintf (STREAM, "\tmov.l\t1f,r1\n");				\
    fprintf (STREAM, "\tsts.l\tpr,@-r15\n");				\
    fprintf (STREAM, "\tmova\t2f,r0\n");				\
    fprintf (STREAM, "\tjmp\t@r1\n");					\
    fprintf (STREAM, "\tlds\tr0,pr\n");					\
    fprintf (STREAM, "\t.align\t2\n");					\
    if (flag_pic)							\
      {									\
	fprintf (STREAM, "1:\t.long\tmcount@GOT\n");			\
	fprintf (STREAM, "3:\t.long\t_GLOBAL_OFFSET_TABLE_\n");		\
      }									\
    else								\
      fprintf (STREAM, "1:\t.long\tmcount\n");				\
    fprintf (STREAM, "2:\tlds.l\t@r15+,pr\n");				\
  } while (0)

#define MD_UNWIND_SUPPORT "config/sh/linux-unwind.h"

/* For SH3 and SH4, we use a slot of the unwind frame which correspond
   to a fake register number 16 as a placeholder for the return address
   in MD_FALLBACK_FRAME_STATE_FOR and its content will be read with
   _Unwind_GetGR which uses dwarf_reg_size_table to get the size of
   the register.  So the entry of dwarf_reg_size_table corresponding to
   this slot must be set.  To do this, we redefine DBX_REGISTER_NUMBER
   so as to return itself for 16.  */
#undef DBX_REGISTER_NUMBER
#define DBX_REGISTER_NUMBER(REGNO) \
  ((! TARGET_SH5 && (REGNO) == 16) ? 16 : SH_DBX_REGISTER_NUMBER (REGNO))
