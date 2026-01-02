/* An enum used to discriminate severities of diagnostics.
   Copyright (C) 1998-2026 Free Software Foundation, Inc.

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

#ifndef GCC_DIAGNOSTICS_KINDS_H
#define GCC_DIAGNOSTICS_KINDS_H

namespace diagnostics {

/* Constants used to discriminate diagnostics.  */
enum class kind
{
#define DEFINE_DIAGNOSTIC_KIND(K, msgid, C) K,
#include "diagnostics/kinds.def"
#undef DEFINE_DIAGNOSTIC_KIND
  last_diagnostic_kind,
  /* This is used for tagging pragma pops in the diagnostic
     classification history chain.  */
  pop,
  /* This is used internally to note that a diagnostic is enabled
     without mandating any specific type.  */
  any
};

extern const char *get_text_for_kind (enum diagnostics::kind);
extern const char *get_debug_string_for_kind (enum diagnostics::kind);
extern const char *get_color_for_kind (enum diagnostics::kind);

} // namespace diagnostics

#endif /* ! GCC_DIAGNOSTICS_KINDS_H */
