/* Definitions for Intel 386 systems using GNU userspace.
   Copyright (C) 1994, 1995, 1996, 1997, 1998, 1999, 2001, 2002, 2004, 2005,
   2006, 2007, 2008, 2009, 2010, 2011 Free Software Foundation, Inc.
   Contributed by Eric Youngdale.
   Modified for stabs-in-ELF by H.J. Lu.

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

/* Output at beginning of assembler file.  */
/* The .file command should always begin the output.  */
#define TARGET_ASM_FILE_START_FILE_DIRECTIVE true

/* The svr4 ABI for the i386 says that records and unions are returned
   in memory.  */
#undef DEFAULT_PCC_STRUCT_RETURN
#define DEFAULT_PCC_STRUCT_RETURN 1

/* We arrange for the whole %gs segment to map the tls area.  */
#undef TARGET_TLS_DIRECT_SEG_REFS_DEFAULT
#define TARGET_TLS_DIRECT_SEG_REFS_DEFAULT MASK_TLS_DIRECT_SEG_REFS

#undef ASM_COMMENT_START
#define ASM_COMMENT_START "#"

#undef DBX_REGISTER_NUMBER
#define DBX_REGISTER_NUMBER(n) \
  (TARGET_64BIT ? dbx64_register_map[n] : svr4_dbx_register_map[n])

/* Output assembler code to FILE to call the profiler.
   To the best of my knowledge, no GNU userspace libc has required the label
   argument to mcount.  */

#define NO_PROFILE_COUNTERS	1

#undef MCOUNT_NAME
#define MCOUNT_NAME "mcount"

/* The GLIBC version of mcount for the x86 assumes that there is a
   frame, so we cannot allow profiling without a frame pointer.  */

#undef SUBTARGET_FRAME_POINTER_REQUIRED
#define SUBTARGET_FRAME_POINTER_REQUIRED crtl->profile

#undef SIZE_TYPE
#define SIZE_TYPE "unsigned int"
 
#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE "int"
  
#undef WCHAR_TYPE
#define WCHAR_TYPE "long int"
   
#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE BITS_PER_WORD
    
#define TARGET_OS_CPP_BUILTINS()		\
  do						\
    {						\
	GNU_USER_TARGET_OS_CPP_BUILTINS();	\
    }						\
  while (0)

#undef CPP_SPEC
#define CPP_SPEC "%{posix:-D_POSIX_SOURCE} %{pthread:-D_REENTRANT}"

#undef CC1_SPEC
#define CC1_SPEC "%(cc1_cpu) %{profile:-p}"

/* Provide a LINK_SPEC appropriate for GNU userspace.  Here we provide support
   for the special GCC options -static and -shared, which allow us to
   link things in one of these three modes by applying the appropriate
   combinations of options at link-time.

   When the -shared link option is used a final link is not being
   done.  */

#undef  ASM_SPEC
#define ASM_SPEC \
  "--32 %{!mno-sse2avx:%{mavx:-msse2avx}} %{msse2avx:%{!mavx:-msse2avx}}"

#undef  SUBTARGET_EXTRA_SPECS
#define SUBTARGET_EXTRA_SPECS \
  { "link_emulation", GNU_USER_LINK_EMULATION },\
  { "dynamic_linker", GNU_USER_DYNAMIC_LINKER }

#undef	LINK_SPEC
#define LINK_SPEC "-m %(link_emulation) %{shared:-shared} \
  %{!shared: \
    %{!static: \
      %{rdynamic:-export-dynamic} \
      -dynamic-linker %(dynamic_linker)} \
      %{static:-static}}"

/* Similar to standard GNU userspace, but adding -ffast-math support.  */
#undef  ENDFILE_SPEC
#define ENDFILE_SPEC \
  "%{Ofast|ffast-math|funsafe-math-optimizations:crtfastmath.o%s} \
   %{mpc32:crtprec32.o%s} \
   %{mpc64:crtprec64.o%s} \
   %{mpc80:crtprec80.o%s} \
   %{shared|pie:crtendS.o%s;:crtend.o%s} crtn.o%s"

/* A C statement (sans semicolon) to output to the stdio stream
   FILE the assembler definition of uninitialized global DECL named
   NAME whose size is SIZE bytes and alignment is ALIGN bytes.
   Try to use asm_output_aligned_bss to implement this macro.  */

#define ASM_OUTPUT_ALIGNED_BSS(FILE, DECL, NAME, SIZE, ALIGN) \
  asm_output_aligned_bss (FILE, DECL, NAME, SIZE, ALIGN)

/* A C statement to output to the stdio stream FILE an assembler
   command to advance the location counter to a multiple of 1<<LOG
   bytes if it is within MAX_SKIP bytes.

   This is used to align code labels according to Intel recommendations.  */

#ifdef HAVE_GAS_MAX_SKIP_P2ALIGN
#define ASM_OUTPUT_MAX_SKIP_ALIGN(FILE,LOG,MAX_SKIP)			\
  do {									\
    if ((LOG) != 0) {							\
      if ((MAX_SKIP) == 0) fprintf ((FILE), "\t.p2align %d\n", (LOG));	\
      else {								\
	fprintf ((FILE), "\t.p2align %d,,%d\n", (LOG), (MAX_SKIP));	\
	/* Make sure that we have at least 8 byte alignment if > 8 byte \
	   alignment is preferred.  */					\
	if ((LOG) > 3							\
	    && (1 << (LOG)) > ((MAX_SKIP) + 1)				\
	    && (MAX_SKIP) >= 7)						\
	  fputs ("\t.p2align 3\n", (FILE));				\
      }									\
    }									\
  } while (0)
#endif

/* Handle special EH pointer encodings.  Absolute, pc-relative, and
   indirect are handled automatically.  */
#define ASM_MAYBE_OUTPUT_ENCODED_ADDR_RTX(FILE, ENCODING, SIZE, ADDR, DONE) \
  do {									\
    if ((SIZE) == 4 && ((ENCODING) & 0x70) == DW_EH_PE_datarel)		\
      {									\
        fputs (ASM_LONG, FILE);			\
        assemble_name (FILE, XSTR (ADDR, 0));				\
	fputs (((ENCODING) & DW_EH_PE_indirect ? "@GOT" : "@GOTOFF"), FILE); \
        goto DONE;							\
      }									\
  } while (0)

/* Used by crtstuff.c to initialize the base of data-relative relocations.
   These are GOT relative on x86, so return the pic register.  */
#ifdef __PIC__
#define CRT_GET_RFIB_DATA(BASE)			\
  {						\
    register void *ebx_ __asm__("ebx");		\
    BASE = ebx_;				\
  }
#else
#define CRT_GET_RFIB_DATA(BASE)						\
  __asm__ ("call\t.LPR%=\n"						\
	   ".LPR%=:\n\t"						\
	   "pop{l}\t%0\n\t"						\
	   /* Due to a GAS bug, this cannot use EAX.  That encodes	\
	      smaller than the traditional EBX, which results in the	\
	      offset being off by one.  */				\
	   "add{l}\t{$_GLOBAL_OFFSET_TABLE_+[.-.LPR%=],%0"		\
		   "|%0,_GLOBAL_OFFSET_TABLE_+(.-.LPR%=)}"		\
	   : "=d"(BASE))
#endif

/* Put all *tf routines in libgcc.  */
#undef LIBGCC2_HAS_TF_MODE
#define LIBGCC2_HAS_TF_MODE 1
#define LIBGCC2_TF_CEXT q
#define TF_SIZE 113

#define TARGET_ASM_FILE_END file_end_indicate_exec_stack

/* The stack pointer needs to be moved while checking the stack.  */
#define STACK_CHECK_MOVING_SP 1

/* Static stack checking is supported by means of probes.  */
#define STACK_CHECK_STATIC_BUILTIN 1

#ifdef TARGET_LIBC_PROVIDES_SSP
/* i386 glibc provides __stack_chk_guard in %gs:0x14.  */
#define TARGET_THREAD_SSP_OFFSET	0x14

/* We steal the last transactional memory word.  */
#define TARGET_CAN_SPLIT_STACK
#define TARGET_THREAD_SPLIT_STACK_OFFSET 0x30
#endif
