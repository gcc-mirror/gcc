/* Definitions of target machine for GCC.  VxWorks i586 version.
   Copyright (C) 2003 Free Software Foundation, Inc.

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


#define HANDLE_SYSV_PRAGMA 1

#undef  TARGET_VERSION
#define TARGET_VERSION fprintf (stderr, " (80586, VxWorks syntax)");

/* Prefix for internally generated assembler labels.  If we aren't using
   underscores, we are using prefix `.'s to identify labels that should
   be ignored, as in `i386/gas.h' --karl@cs.umb.edu  */

#define LPREFIX "L"

/* Assembler pseudos to introduce constants of various size.  */

#define ASM_SHORT "\t.word\t"
#define ASM_LONG "\t.long\t"
#define ASM_QUAD "\t.quad\t"  /* Should not be used for 32bit compilation.  */


#define ASM_OUTPUT_ALIGN(FILE,LOG) \
  if ((LOG)!=0) fprintf ((FILE), "\t.balign %d\n", 1<<(LOG))

#undef  ASM_SPEC
#define ASM_SPEC "%{v:-V} %{Qy:} %{n} %{T} %{Ym,*} %{Yd,*} %{Wa,*:%*}"

#define TARGET_OS_CPP_BUILTINS()                        \
  do                                                    \
    {                                                   \
      builtin_define ("__vxworks");                     \
      builtin_assert ("system=unix");                   \
                                                        \
      if (TARGET_386)                                   \
        builtin_define ("CPU=I80386");                  \
      else if (TARGET_486)                              \
        builtin_define ("CPU=I80486");                  \
      else if (TARGET_PENTIUM)                          \
        {                                               \
          builtin_define ("CPU=PENTIUM");               \
          builtin_define ("CPU_VARIANT=PENTIUM");       \
        }                                               \
      else if (TARGET_PENTIUMPRO)                       \
        {                                               \
          builtin_define ("CPU=PENTIUM2");               \
          builtin_define ("CPU_VARIANT=PENTIUMPRO");    \
        }                                               \
      else if (TARGET_PENTIUM4)                       \
        {                                               \
          builtin_define ("CPU=PENTIUM4");               \
          builtin_define ("CPU_VARIANT=PENTIUM4");    \
        }                                               \
    }                                                   \
  while (0)


