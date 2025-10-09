/* Machine description for AArch64 MS ABI.
   Copyright (C) 2024-2025 Free Software Foundation, Inc.

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

#ifndef GCC_AARCH64_ABI_MS_H
#define GCC_AARCH64_ABI_MS_H

/* X18 reserved for the TEB on Windows.  */

#undef FIXED_X18
#define FIXED_X18 1

#undef CALL_USED_X18
#define CALL_USED_X18 0

#undef  STATIC_CHAIN_REGNUM
#define STATIC_CHAIN_REGNUM R17_REGNUM

#define ASM_COMMENT_START "//"

/* ASM_OUTPUT_TYPE_DIRECTIVE is not yet supported by binutils for the
   aarch64-w64-mingw32 target.  */
#define ASM_OUTPUT_TYPE_DIRECTIVE(STREAM, NAME, TYPE)

/* Structured Exception Handling (SEH) is not yet supported by binutils
   so adding seh_endproc as an assembly comment to mark the end of a
   function.  */
#define ASM_DECLARE_FUNCTION_SIZE(FILE, FNAME, DECL) \
  fprintf (FILE, "\t" ASM_COMMENT_START "  seh_endproc\n")

/* Long double is 64 bit for Coff targets.
   Reference:
   https://learn.microsoft.com/en-us/cpp/c-language/type-long-double.  */
#undef TARGET_LONG_DOUBLE_128
#define TARGET_LONG_DOUBLE_128 0

#endif /* GCC_AARCH64_ABI_MS_H.  */
