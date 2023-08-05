/* Base class for svalues and regions.
   Copyright (C) 2023 Free Software Foundation, Inc.
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

#include "config.h"
#define INCLUDE_MEMORY
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "analyzer/analyzer.h"
#include "analyzer/symbol.h"

#if ENABLE_ANALYZER

namespace ana {

/* Compare SYM1 and SYM2 by id.  */

int
symbol::cmp_ids (const symbol *sym1, const symbol *sym2)
{
  return (long)sym1->get_id () - (long)sym2->get_id ();
}

} // namespace ana

#endif /* #if ENABLE_ANALYZER */
