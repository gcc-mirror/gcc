/* Definitions for LynxOS on i386.
   Copyright (C) 1993-2024 Free Software Foundation, Inc.

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

#define TARGET_OS_CPP_BUILTINS()		\
  do						\
    {						\
      builtin_define ("__LITTLE_ENDIAN__");	\
      builtin_define ("__x86__");		\
    }						\
  while (0)

/* The svr4 ABI for the i386 says that records and unions are returned
   in memory.  */

#define DEFAULT_PCC_STRUCT_RETURN 1

/* BSS_SECTION_ASM_OP gets defined i386/unix.h.  */

#define ASM_OUTPUT_ALIGNED_BSS(FILE, DECL, NAME, SIZE, ALIGN) \
  asm_output_aligned_bss (FILE, DECL, NAME, SIZE, ALIGN)

/* LynxOS's GDB counts the floating point registers from 16.  */

#undef DEBUGGER_REGNO
#define DEBUGGER_REGNO(n)						\
  (TARGET_64BIT ? debugger64_register_map[n]					\
   : (n) == 0 ? 0							\
   : (n) == 1 ? 2							\
   : (n) == 2 ? 1							\
   : (n) == 3 ? 3							\
   : (n) == 4 ? 6							\
   : (n) == 5 ? 7							\
   : (n) == 6 ? 5							\
   : (n) == 7 ? 4							\
   : ((n) >= FIRST_STACK_REG && (n) <= LAST_STACK_REG) ? (int) (n) + 8	\
   : (-1))

/* Undefine SUBTARGET_EXTRA_SPECS it is empty anyway.  We define it in
   config/lynx.h.  */

#undef SUBTARGET_EXTRA_SPECS

/* Undefine the definition from att.h to enable our default.  */

#undef ASM_OUTPUT_ALIGN

/* The file i386.cc defines TARGET_HAVE_TLS unconditionally if
   HAVE_AS_TLS is defined.  HAVE_AS_TLS is defined as gas support for
   TLS is detected by configure.  We undefine it here.  */

#undef HAVE_AS_TLS
