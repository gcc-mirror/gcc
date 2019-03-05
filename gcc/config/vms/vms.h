/* Definitions of target machine GNU compiler. VMS common version.
   Copyright (C) 2003-2019 Free Software Foundation, Inc.
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

#define TARGET_OS_CPP_BUILTINS()					 \
  do {									 \
    builtin_define_std ("vms");						 \
    builtin_define_std ("VMS");						 \
    builtin_assert ("system=vms");					 \
    SUBTARGET_OS_CPP_BUILTINS();					 \
    builtin_define ("__int64=long long");				 \
    if (flag_vms_pointer_size == VMS_POINTER_SIZE_32)			 \
      builtin_define ("__INITIAL_POINTER_SIZE=32");			 \
    else if (flag_vms_pointer_size == VMS_POINTER_SIZE_64)		 \
      builtin_define ("__INITIAL_POINTER_SIZE=64");			 \
    if (POINTER_SIZE == 64)						 \
      builtin_define ("__LONG_POINTERS=1");				 \
    builtin_define_with_int_value ("__CRTL_VER", vms_c_get_crtl_ver ()); \
    builtin_define_with_int_value ("__VMS_VER", vms_c_get_vms_ver ());   \
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
#define POINTER_SIZE (flag_vms_pointer_size == VMS_POINTER_SIZE_NONE ? 32 : 64)
#define POINTERS_EXTEND_UNSIGNED 0

/* Always a 32 bit type.  */
#undef SIZE_TYPE
#define SIZE_TYPE  "unsigned int"

/* ???: Defined as a 'int' by dec-c, but obstack.h doesn't like it.  */
#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE (flag_vms_pointer_size == VMS_POINTER_SIZE_NONE ? \
                      "int" : "long long int")

#define SIZETYPE (flag_vms_pointer_size == VMS_POINTER_SIZE_NONE ? \
		  "unsigned int" : "long long unsigned int")

#define C_COMMON_OVERRIDE_OPTIONS vms_c_common_override_options ()

/* VMS doesn't support other sections than .text for code.  */
#define TARGET_ASM_FUNCTION_SECTION vms_function_section

/* Always use 8 bytes addresses in dwarf2 debug info.  The default value doesn't
   work as it may be 4 bytes, which won't match gas default (8 bytes for ia64),
   and will thus produce incorrect values.  */
#define DWARF2_ADDR_SIZE 8

/* No libm on VMS.  */
#define MATH_LIBRARY ""

/* Special VMS debugger symbol to record the entry point.  */
#define VMS_DEBUG_MAIN_POINTER "TRANSFER$BREAK$GO"

#undef TARGET_LIBC_HAS_FUNCTION
#define TARGET_LIBC_HAS_FUNCTION no_c99_libc_has_function
