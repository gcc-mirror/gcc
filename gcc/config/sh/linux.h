/* Definitions for SH running Linux-based GNU systems using ELF
   Copyright (C) 1999-2020 Free Software Foundation, Inc.
   Contributed by Kazumoto Kojima <kkojima@rr.iij4u.or.jp>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

/* Run-time Target Specification.  */

/* Enable DWARF 2 exceptions.  */
#undef DWARF2_UNWIND_INFO
#define DWARF2_UNWIND_INFO 1

#undef SUBTARGET_CPP_SPEC
#define SUBTARGET_CPP_SPEC "\
   %{posix:-D_POSIX_SOURCE} \
   %{pthread:-D_REENTRANT -D_PTHREADS} \
"

#define TARGET_OS_CPP_BUILTINS() \
  do						\
    {						\
      GNU_USER_TARGET_OS_CPP_BUILTINS();	\
    }						\
  while (0)

#undef TARGET_DEFAULT
#define TARGET_DEFAULT \
  (TARGET_CPU_DEFAULT | TARGET_ENDIAN_DEFAULT | TARGET_OPT_DEFAULT)

#define TARGET_ASM_FILE_END file_end_indicate_exec_stack

#if TARGET_ENDIAN_DEFAULT == MASK_LITTLE_ENDIAN
#define MUSL_DYNAMIC_LINKER_E "%{mb:eb}"
#else
#define MUSL_DYNAMIC_LINKER_E "%{!ml:eb}"
#endif

#if TARGET_CPU_DEFAULT & (MASK_HARD_SH2A_DOUBLE | MASK_SH4)
/* "-nofpu" if any nofpu option is specified.  */
#define MUSL_DYNAMIC_LINKER_FP \
  "%{m1|m2|m2a-nofpu|m3|m4-nofpu|m4-100-nofpu|m4-200-nofpu|m4-300-nofpu|" \
  "m4-340|m4-400|m4-500|m4al:-nofpu}"
#else
/* "-nofpu" if none of the hard fpu options are specified.  */
#define MUSL_DYNAMIC_LINKER_FP "%{m2a|m4|m4-100|m4-200|m4-300|m4a:;:-nofpu}"
#endif

#undef MUSL_DYNAMIC_LINKER
#define MUSL_DYNAMIC_LINKER \
  "/lib/ld-musl-sh" MUSL_DYNAMIC_LINKER_E MUSL_DYNAMIC_LINKER_FP \
  "%{mfdpic:-fdpic}.so.1"

#define GLIBC_DYNAMIC_LINKER "/lib/ld-linux.so.2"

#undef SUBTARGET_LINK_EMUL_SUFFIX
#define SUBTARGET_LINK_EMUL_SUFFIX "%{mfdpic:_fd;:_linux}"

#undef SUBTARGET_LINK_SPEC
#define SUBTARGET_LINK_SPEC \
  "%{shared:-shared} \
   %{!static: \
     %{rdynamic:-export-dynamic} \
     -dynamic-linker " GNU_USER_DYNAMIC_LINKER "} \
   %{static:-static}"

/* Output assembler code to STREAM to call the profiler.  */
#undef FUNCTION_PROFILER
#define FUNCTION_PROFILER(STREAM,LABELNO)				\
  do {									\
	if (flag_pic)							\
	  {								\
	    fprintf (STREAM, "\tmov.l\t3f,r1\n");			\
	    fprintf (STREAM, "\tmova\t3f,r0\n");			\
	    fprintf (STREAM, "\tadd\tr1,r0\n");				\
	    fprintf (STREAM, "\tmov.l\t1f,r1\n");			\
	    fprintf (STREAM, "\tmov.l\t@(r0,r1),r1\n");			\
	  }								\
	else								\
	  fprintf (STREAM, "\tmov.l\t1f,r1\n");				\
	fprintf (STREAM, "\tsts.l\tpr,@-r15\n");			\
	fprintf (STREAM, "\tmova\t2f,r0\n");				\
	fprintf (STREAM, "\tjmp\t@r1\n");				\
	fprintf (STREAM, "\tlds\tr0,pr\n");				\
	fprintf (STREAM, "\t.align\t2\n");				\
	if (flag_pic)							\
	  {								\
	    fprintf (STREAM, "1:\t.long\tmcount@GOT\n");		\
	    fprintf (STREAM, "3:\t.long\t_GLOBAL_OFFSET_TABLE_\n");	\
	  }								\
	else								\
	  fprintf (STREAM, "1:\t.long\tmcount\n");			\
	fprintf (STREAM, "2:\tlds.l\t@r15+,pr\n");			\
  } while (0)

/* For SH3 and SH4, we use a slot of the unwind frame which correspond
   to a fake register number 16 as a placeholder for the return address
   in MD_FALLBACK_FRAME_STATE_FOR and its content will be read with
   _Unwind_GetGR which uses dwarf_reg_size_table to get the size of
   the register.  So the entry of dwarf_reg_size_table corresponding to
   this slot must be set.  To do this, we redefine DBX_REGISTER_NUMBER
   so as to return itself for 16.  */
#undef DBX_REGISTER_NUMBER
#define DBX_REGISTER_NUMBER(REGNO) \
  (((REGNO) == 16) ? 16 : SH_DBX_REGISTER_NUMBER (REGNO))

/* Install the __sync libcalls.  */
#undef TARGET_INIT_LIBFUNCS
#define TARGET_INIT_LIBFUNCS  sh_init_sync_libfuncs

#undef SUBTARGET_OVERRIDE_OPTIONS
#define SUBTARGET_OVERRIDE_OPTIONS					\
  do									\
    {									\
      /* Set default atomic model if it hasn't been specified.  */	\
      if (global_options_set.x_sh_atomic_model_str == 0)		\
	{								\
	  if (TARGET_SH3)						\
	    sh_atomic_model_str = "soft-gusa";				\
	  else if (TARGET_SH1)						\
	    sh_atomic_model_str = "soft-imask";				\
	}								\
      /* Set -musermode if it hasn't been specified.  */		\
      if (global_options_set.x_TARGET_USERMODE == 0)			\
	TARGET_USERMODE = true;						\
    }									\
  while (0)
