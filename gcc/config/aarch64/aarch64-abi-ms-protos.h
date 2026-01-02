/* Windows specific ABI for AArch64 architecture.
   Copyright (C) 2025-2026 Free Software Foundation, Inc.
   Contributed by ARM Ltd.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#ifndef GCC_AARCH64_ABI_MS_PROTOS_H
#define GCC_AARCH64_ABI_MS_PROTOS_H

extern int aarch64_ms_variadic_abi_enum_va_list (int, const char **,
						 tree *ptree);

extern tree aarch64_ms_variadic_abi_fn_abi_va_list (tree fndecl);

extern tree aarch64_ms_variadic_abi_canonical_va_list_type (tree type);

extern int aarch64_arg_partial_bytes (cumulative_args_t,
				      const function_arg_info &);

#endif
