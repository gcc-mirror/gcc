/* Code for unwinding macro expansions in diagnostics.
   Copyright (C) 2000-2026 Free Software Foundation, Inc.

This file is part of GCC.

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
<http://www.gnu.org/licenses/>.  */

#ifndef GCC_DIAGNOSTICS_MACRO_UNWINDING_H
#define GCC_DIAGNOSTICS_MACRO_UNWINDING_H

namespace diagnostics {

struct diagnostic_info;

extern void virt_loc_aware_text_finalizer (text_sink &,
					   const diagnostic_info *);

extern void maybe_unwind_expanded_macro_loc (text_sink &,
					     location_t where);

} // namespace diagnostics

#endif /* ! GCC_DIAGNOSTICS_MACRO_UNWINDING_H */
