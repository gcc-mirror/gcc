/* Expand a SYMBOL into its corresponding dllimport, far-address,
or refptr symbol.
Copyright (C) 2024-2025 Free Software Foundation, Inc.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
http://www.gnu.org/licenses/.  */

#ifndef GCC_MINGW_WINNT_DLL_H
#define GCC_MINGW_WINNT_DLL_H

#ifndef USED_FOR_TARGET

extern bool is_imported_p (rtx x);
extern alias_set_type ix86_GOT_alias_set (void);
extern alias_set_type mingw_GOT_alias_set (void);
extern rtx legitimize_pe_coff_symbol (rtx addr, bool inreg);

#endif /* not USED_FOR_TARGET.  */

#endif /* GCC_MINGW_WINNT_DLL_H.  */
