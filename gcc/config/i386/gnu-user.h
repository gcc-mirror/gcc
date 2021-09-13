/* Definitions for Intel 386 systems using GNU userspace.
   Copyright (C) 1994-2021 Free Software Foundation, Inc.
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

#undef SIZE_TYPE
#define SIZE_TYPE "unsigned int"
 
#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE "int"
  
#undef WCHAR_TYPE
#define WCHAR_TYPE "long int"
   
#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE BITS_PER_WORD
    
/* Provide a LINK_SPEC appropriate for GNU userspace.  Here we provide support
   for the special GCC options -static and -shared, which allow us to
   link things in one of these three modes by applying the appropriate
   combinations of options at link-time.

   When the -shared link option is used a final link is not being
   done.  */

#undef  ASM_SPEC
#define ASM_SPEC \
  "--32 %{msse2avx:%{!mavx:-msse2avx}}"

#undef  SUBTARGET_EXTRA_SPECS
#define SUBTARGET_EXTRA_SPECS \
  { "link_emulation", GNU_USER_LINK_EMULATION },\
  { "dynamic_linker", GNU_USER_DYNAMIC_LINKER }

#define GNU_USER_TARGET_LINK_SPEC "-m %(link_emulation) %{shared:-shared} \
  %{!shared: \
    %{!static: \
      %{!static-pie: \
	%{rdynamic:-export-dynamic} \
	-dynamic-linker %(dynamic_linker)}} \
    %{static:-static} %{static-pie:-static -pie --no-dynamic-linker -z text}}"

#undef	LINK_SPEC
#define LINK_SPEC GNU_USER_TARGET_LINK_SPEC

/* A C statement (sans semicolon) to output to the stdio stream
   FILE the assembler definition of uninitialized global DECL named
   NAME whose size is SIZE bytes and alignment is ALIGN bytes.
   Try to use asm_output_aligned_bss to implement this macro.  */

#define ASM_OUTPUT_ALIGNED_BSS(FILE, DECL, NAME, SIZE, ALIGN) \
  asm_output_aligned_bss (FILE, DECL, NAME, SIZE, ALIGN)

/* A C statement to output to the stdio stream FILE an assembler
   command to advance the location counter to a multiple of 1<<LOG
   bytes if it is within MAX_SKIP bytes.  */

#define SUBALIGN_LOG 3

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

#ifdef TARGET_LIBC_PROVIDES_SSP
/* i386 glibc provides __stack_chk_guard in %gs:0x14.  */
#define TARGET_THREAD_SSP_OFFSET	0x14

/* i386 glibc provides __private_ss in %gs:0x30.  */
#define TARGET_THREAD_SPLIT_STACK_OFFSET 0x30
#endif
