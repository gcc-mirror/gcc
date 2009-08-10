/* Output variables, constants and external declarations, for GNU compiler.
   Copyright (C) 2001, 2007, 2009 Free Software Foundation, Inc.
   Contributed by Douglas Rupp (rupp@gnat.com).

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

#undef TARGET_OS_CPP_BUILTINS
#define TARGET_OS_CPP_BUILTINS()                \
    do {                                        \
        builtin_define_std ("vms");             \
        builtin_define_std ("VMS");             \
        builtin_define ("__ALPHA");             \
        builtin_assert ("system=vms");          \
        builtin_define ("__IEEE_FLOAT");        \
        builtin_define ("__LONG_POINTERS=1");   \
    } while (0)

#undef SUBTARGET_SWITCHES
#define SUBTARGET_SWITCHES \
  { "malloc64",    MASK_MALLOC64,     "Malloc data into P2 space" },

#undef TARGET_DEFAULT
#define TARGET_DEFAULT (MASK_FPREGS | MASK_GAS | MASK_MALLOC64)

#undef LONG_TYPE_SIZE
#define LONG_TYPE_SIZE 64

#undef POINTER_SIZE
#define POINTER_SIZE 64

/* Eventhough pointers are 64bits, only 32bit ever remain significant in code
   addresses.  */
#define MASK_RETURN_ADDR (GEN_INT (0xffffffff))

/* Defaults to "long int" */
#undef SIZE_TYPE
#undef PTRDIFF_TYPE

# include "config/vms/vms-crtl-64.h"
