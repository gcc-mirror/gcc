/* Definitions for Intel 386 running Linux-based GNU systems with ELF format.
   Copyright (C) 1994, 1995, 1996, 1997, 1998, 1999, 2001, 2002
   Free Software Foundation, Inc.
   Contributed by Eric Youngdale.
   Modified for stabs-in-ELF by H.J. Lu.

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

#define LINUX_DEFAULT_ELF

/* Output at beginning of assembler file.  */
/* The .file command should always begin the output.  */
#undef ASM_FILE_START
#define ASM_FILE_START(FILE)						\
  do {									\
	output_file_directive (FILE, main_input_filename);		\
	if (ix86_asm_dialect == ASM_INTEL)				\
	  fputs ("\t.intel_syntax\n", FILE);				\
  } while (0)

#undef TARGET_VERSION
#define TARGET_VERSION fprintf (stderr, " (i386 Linux/ELF)");

/* The svr4 ABI for the i386 says that records and unions are returned
   in memory.  */
#undef DEFAULT_PCC_STRUCT_RETURN
#define DEFAULT_PCC_STRUCT_RETURN 1

#undef ASM_COMMENT_START
#define ASM_COMMENT_START "#"

#undef DBX_REGISTER_NUMBER
#define DBX_REGISTER_NUMBER(n) \
  (TARGET_64BIT ? dbx64_register_map[n] : svr4_dbx_register_map[n])

/* Output assembler code to FILE to call the profiler.
   To the best of my knowledge, no Linux libc has required the label
   argument to mcount.  */

#define NO_PROFILE_COUNTERS

#undef FUNCTION_PROFILER
#define FUNCTION_PROFILER(FILE, LABELNO)  \
{									\
  if (flag_pic)								\
    fprintf (FILE, "\tcall\t*mcount@GOT(%%ebx)\n");			\
  else									\
    fprintf (FILE, "\tcall\tmcount\n");					\
}

/* True if it is possible to profile code that does not have a frame
   pointer.  

   The GLIBC version of mcount for the x86 assumes that there is a
   frame, so we cannot allow profiling without a frame pointer.  */

#undef TARGET_ALLOWS_PROFILING_WITHOUT_FRAME_POINTER
#define TARGET_ALLOWS_PROFILING_WITHOUT_FRAME_POINTER false

#undef SIZE_TYPE
#define SIZE_TYPE "unsigned int"
 
#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE "int"
  
#undef WCHAR_TYPE
#define WCHAR_TYPE "long int"
   
#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE BITS_PER_WORD
    
#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-D__ELF__ -Dunix -Dlinux -Asystem=posix"

#undef CPP_SPEC
#ifdef USE_GNULIBC_1
#define CPP_SPEC "%(cpp_cpu) %{fPIC:-D__PIC__ -D__pic__} %{fpic:-D__PIC__ -D__pic__} %{posix:-D_POSIX_SOURCE}"
#else
#define CPP_SPEC "%(cpp_cpu) %{fPIC:-D__PIC__ -D__pic__} %{fpic:-D__PIC__ -D__pic__} %{posix:-D_POSIX_SOURCE} %{pthread:-D_REENTRANT}"
#endif

#undef CC1_SPEC
#define CC1_SPEC "%(cc1_cpu) %{profile:-p}"

/* Provide a LINK_SPEC appropriate for Linux.  Here we provide support
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

#undef	LINK_SPEC
#ifdef USE_GNULIBC_1
#ifndef LINUX_DEFAULT_ELF
#define LINK_SPEC "-m elf_i386 %{shared:-shared} \
  %{!shared: \
    %{!ibcs: \
      %{!static: \
	%{rdynamic:-export-dynamic} \
	%{!dynamic-linker:-dynamic-linker /lib/elf/ld-linux.so.1} \
	%{!rpath:-rpath /lib/elf/}} %{static:-static}}}"
#else
#define LINK_SPEC "-m elf_i386 %{shared:-shared} \
  %{!shared: \
    %{!ibcs: \
      %{!static: \
	%{rdynamic:-export-dynamic} \
	%{!dynamic-linker:-dynamic-linker /lib/ld-linux.so.1}} \
	%{static:-static}}}"
#endif
#else
#define LINK_SPEC "-m elf_i386 %{shared:-shared} \
  %{!shared: \
    %{!ibcs: \
      %{!static: \
	%{rdynamic:-export-dynamic} \
	%{!dynamic-linker:-dynamic-linker /lib/ld-linux.so.2}} \
	%{static:-static}}}"
#endif

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
      else fprintf ((FILE), "\t.p2align %d,,%d\n", (LOG), (MAX_SKIP));	\
    }									\
  } while (0)
#endif

#if defined(__PIC__) && defined (USE_GNULIBC_1)
/* This is a kludge. The i386 GNU/Linux dynamic linker needs ___brk_addr,
   __environ and atexit.  We have to make sure they are in the .dynsym
   section.  We do this by forcing the assembler to create undefined 
   references to these symbols in the object file.  */
#undef CRT_CALL_STATIC_FUNCTION
#define CRT_CALL_STATIC_FUNCTION(SECTION_OP, FUNC)	\
   asm (SECTION_OP "\n\t"				\
	"call " USER_LABEL_PREFIX #FUNC "\n"		\
	TEXT_SECTION_ASM_OP "\n\t"			\
	".extern ___brk_addr\n\t"			\
	".type ___brk_addr,@object\n\t"			\
	".extern __environ\n\t"				\
	".type __environ,@object\n\t"			\
	".extern atexit\n\t"				\
	".type atexit,@function");
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
	   "popl\t%0\n\t"						\
	   /* Due to a GAS bug, this cannot use EAX.  That encodes	\
	      smaller than the traditional EBX, which results in the	\
	      offset being off by one.  */				\
	   "addl\t$_GLOBAL_OFFSET_TABLE_+[.-.LPR%=],%0"			\
	   : "=d"(BASE))
#endif

/* Do code reading to identify a signal frame, and set the frame
   state data appropriately.  See unwind-dw2.c for the structs.  */

#ifdef IN_LIBGCC2
#include <signal.h>
#include <sys/ucontext.h>
#endif

#define MD_FALLBACK_FRAME_STATE_FOR(CONTEXT, FS, SUCCESS)		\
  do {									\
    unsigned char *pc_ = (CONTEXT)->ra;					\
    struct sigcontext *sc_;						\
    long new_cfa_;							\
									\
    /* popl %eax ; movl $__NR_sigreturn,%eax ; int $0x80  */		\
    if (*(unsigned short *)(pc_+0) == 0xb858				\
	&& *(unsigned int *)(pc_+2) == 119				\
	&& *(unsigned short *)(pc_+6) == 0x80cd)			\
      sc_ = (CONTEXT)->cfa + 4;						\
    /* movl $__NR_rt_sigreturn,%eax ; int $0x80  */			\
    else if (*(unsigned char *)(pc_+0) == 0xb8				\
	     && *(unsigned int *)(pc_+1) == 173				\
	     && *(unsigned short *)(pc_+5) == 0x80cd)			\
      {									\
	struct rt_sigframe {						\
	  int sig;							\
	  struct siginfo *pinfo;					\
	  void *puc;							\
	  struct siginfo info;						\
	  struct ucontext uc;						\
	} *rt_ = (CONTEXT)->cfa;					\
	sc_ = (struct sigcontext *) &rt_->uc.uc_mcontext;		\
      }									\
    else								\
      break;								\
									\
    new_cfa_ = sc_->esp;						\
    (FS)->cfa_how = CFA_REG_OFFSET;					\
    (FS)->cfa_reg = 4;							\
    (FS)->cfa_offset = new_cfa_ - (long) (CONTEXT)->cfa;		\
									\
    /* The SVR4 register numbering macros aren't usable in libgcc.  */	\
    (FS)->regs.reg[0].how = REG_SAVED_OFFSET;				\
    (FS)->regs.reg[0].loc.offset = (long)&sc_->eax - new_cfa_;		\
    (FS)->regs.reg[3].how = REG_SAVED_OFFSET;				\
    (FS)->regs.reg[3].loc.offset = (long)&sc_->ebx - new_cfa_;		\
    (FS)->regs.reg[1].how = REG_SAVED_OFFSET;				\
    (FS)->regs.reg[1].loc.offset = (long)&sc_->ecx - new_cfa_;		\
    (FS)->regs.reg[2].how = REG_SAVED_OFFSET;				\
    (FS)->regs.reg[2].loc.offset = (long)&sc_->edx - new_cfa_;		\
    (FS)->regs.reg[6].how = REG_SAVED_OFFSET;				\
    (FS)->regs.reg[6].loc.offset = (long)&sc_->esi - new_cfa_;		\
    (FS)->regs.reg[7].how = REG_SAVED_OFFSET;				\
    (FS)->regs.reg[7].loc.offset = (long)&sc_->edi - new_cfa_;		\
    (FS)->regs.reg[5].how = REG_SAVED_OFFSET;				\
    (FS)->regs.reg[5].loc.offset = (long)&sc_->ebp - new_cfa_;		\
    (FS)->regs.reg[8].how = REG_SAVED_OFFSET;				\
    (FS)->regs.reg[8].loc.offset = (long)&sc_->eip - new_cfa_;		\
    (FS)->retaddr_column = 8;						\
    goto SUCCESS;							\
  } while (0)
