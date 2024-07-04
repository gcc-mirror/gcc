/* Support for virtual clones in symbol table.
   Copyright (C) 2003-2024 Free Software Foundation, Inc.
   Contributed by Jan Hubicka

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "tree.h"
#include "gimple.h"
#include "predict.h"
#include "target.h"
#include "rtl.h"
#include "alloc-pool.h"
#include "cgraph.h"
#include "symbol-summary.h"
#include "symtab-clones.h"
#include "lto-streamer.h"
#include "data-streamer.h"

namespace {

/* Function summary for clone_infos.  */
class GTY((user)) clone_infos_t: public function_summary <clone_info *>
{
public:
  clone_infos_t (symbol_table *table, bool ggc):
    function_summary<clone_info *> (table, ggc) { }
};

}  /* anon namespace  */

/* Return thunk_info possibly creating new one.  */
clone_info *
clone_info::get_create (cgraph_node *node)
{
  if (!symtab->m_clones)
    {
      symtab->m_clones
	 = new (ggc_alloc_no_dtor <function_summary <clone_info *>> ())
	     function_summary <clone_info *> (symtab, true);
      symtab->m_clones->disable_insertion_hook ();
      symtab->m_clones->disable_duplication_hook ();
    }
  return symtab->m_clones->get_create (node);
}
