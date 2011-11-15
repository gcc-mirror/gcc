/* Definitions of target machine GNU compiler. VMS common version.
   Copyright (C) 2003-2009,2011 Free Software Foundation, Inc.
   Contributed by Douglas B Rupp (rupp@gnat.com).

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

#define TARGET_OBJECT_SUFFIX ".obj"
#define TARGET_EXECUTABLE_SUFFIX ".exe"

#define TARGET_OS_CPP_BUILTINS()                     \
  do {                                               \
    builtin_define_std ("vms");                      \
    builtin_define_std ("VMS");                      \
    builtin_assert ("system=vms");                   \
    SUBTARGET_OS_CPP_BUILTINS();                     \
    if (POINTER_SIZE == 64)                          \
      {                                              \
        builtin_define ("__LONG_POINTERS=1");        \
        builtin_define ("__int64=long long");        \
      }                                              \
  } while (0)

extern void vms_c_register_includes (const char *, const char *, int);
#define TARGET_EXTRA_INCLUDES vms_c_register_includes

/* Tell compiler we want to support VMS pragmas */
#define REGISTER_TARGET_PRAGMAS() vms_c_register_pragma ()

/* By default, allow $ to be part of an identifier.  */
#define DOLLARS_IN_IDENTIFIERS 2

#undef TARGET_ABI_OPEN_VMS
#define TARGET_ABI_OPEN_VMS 1

/* "long" is 32 bits, but 64 bits for Ada.  */
#undef LONG_TYPE_SIZE
#define LONG_TYPE_SIZE 32
#define ADA_LONG_TYPE_SIZE 64

/* Pointer is 32 bits but the hardware has 64-bit addresses, sign extended.  */
#undef POINTER_SIZE
#define POINTER_SIZE 32
#define POINTERS_EXTEND_UNSIGNED 0

/* Always 32 bits.  */
#undef SIZE_TYPE
#define SIZE_TYPE "unsigned int"
#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE "int"
