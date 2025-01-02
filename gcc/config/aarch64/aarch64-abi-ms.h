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

#endif /* GCC_AARCH64_ABI_MS_H.  */
