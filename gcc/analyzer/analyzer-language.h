/* Interface between analyzer and frontends.
   Copyright (C) 2022-2024 Free Software Foundation, Inc.
   Contributed by David Malcolm <dmalcolm@redhat.com>.

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

#ifndef GCC_ANALYZER_LANGUAGE_H
#define GCC_ANALYZER_LANGUAGE_H

#include "analyzer/analyzer-logging.h"

#if ENABLE_ANALYZER

namespace ana {

/* Abstract base class for representing a specific TU
   to the analyzer.  */

class translation_unit
{
 public:
  /* Attempt to look up an  value for identifier ID (e.g. in the headers that
     have been seen).  If it is defined and an integer (e.g. either as a
     macro or enum), return the INTEGER_CST value, otherwise return NULL.  */
  virtual tree lookup_constant_by_id (tree id) const = 0;
  virtual tree lookup_type_by_id (tree id) const = 0;
  virtual tree lookup_global_var_by_id (tree id) const = 0;
};

typedef void (*finish_translation_unit_callback)
   (logger *, const translation_unit &);
void register_finish_translation_unit_callback (
    finish_translation_unit_callback callback);

/* Analyzer hook for frontends to call at the end of the TU.  */

void on_finish_translation_unit (const translation_unit &tu);

} // namespace ana

#endif /* #if ENABLE_ANALYZER */

#endif /* GCC_ANALYZER_LANGUAGE_H */
