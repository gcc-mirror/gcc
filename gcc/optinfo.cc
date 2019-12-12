/* Optimization information.
   Copyright (C) 2018-2019 Free Software Foundation, Inc.
   Contributed by David Malcolm <dmalcolm@redhat.com>.

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

#include "optinfo.h"
#include "optinfo-emit-json.h"
#include "dump-context.h"
#include "pretty-print.h"
#include "gimple-pretty-print.h"
#include "cgraph.h"
#include "selftest.h"

/* optinfo_item's ctor.  Takes ownership of TEXT.  */

optinfo_item::optinfo_item (enum optinfo_item_kind kind, location_t location,
			    char *text)
: m_kind (kind), m_location (location), m_text (text)
{
}

/* optinfo_item's dtor.  */

optinfo_item::~optinfo_item ()
{
  free (m_text);
}

/* Get a string from KIND.  */

const char *
optinfo_kind_to_string (enum optinfo_kind kind)
{
  switch (kind)
    {
    default:
      gcc_unreachable ();
    case OPTINFO_KIND_SUCCESS:
      return "success";
    case OPTINFO_KIND_FAILURE:
      return "failure";
    case OPTINFO_KIND_NOTE:
      return "note";
    case OPTINFO_KIND_SCOPE:
      return "scope";
    }
}

/* optinfo's dtor.  */

optinfo::~optinfo ()
{
  /* Cleanup.  */
  unsigned i;
  optinfo_item *item;
  FOR_EACH_VEC_ELT (m_items, i, item)
    delete item;
}

/* Add ITEM to this optinfo.  */

void
optinfo::add_item (optinfo_item *item)
{
  gcc_assert (item);
  m_items.safe_push (item);
}

/* Get MSG_* flags corresponding to KIND.  */

static dump_flags_t
optinfo_kind_to_dump_flag (enum optinfo_kind kind)
{
  switch (kind)
    {
    default:
      gcc_unreachable ();
    case OPTINFO_KIND_SUCCESS:
      return MSG_OPTIMIZED_LOCATIONS;
    case OPTINFO_KIND_FAILURE:
      return MSG_MISSED_OPTIMIZATION;
    case OPTINFO_KIND_NOTE:
    case OPTINFO_KIND_SCOPE:
      return MSG_NOTE;
    }
}

/* Re-emit this optinfo, both to the "non-immediate" destinations,
   *and* to the "immediate" destinations.  */

void
optinfo::emit_for_opt_problem () const
{
  dump_flags_t dump_kind = optinfo_kind_to_dump_flag (get_kind ());
  dump_kind |= MSG_PRIORITY_REEMITTED;

  /* Re-emit to "immediate" destinations, without creating a new optinfo.  */
  dump_context::get ().dump_loc_immediate (dump_kind, get_user_location ());
  unsigned i;
  optinfo_item *item;
  FOR_EACH_VEC_ELT (m_items, i, item)
    dump_context::get ().emit_item (item, dump_kind);

  /* Re-emit to "non-immediate" destinations.  */
  dump_context::get ().emit_optinfo (this);
}

/* Update the optinfo's kind based on DUMP_KIND.  */

void
optinfo::handle_dump_file_kind (dump_flags_t dump_kind)
{
  /* Any optinfo for a "scope" should have been emitted separately.  */
  gcc_assert (m_kind != OPTINFO_KIND_SCOPE);

  if (dump_kind & MSG_OPTIMIZED_LOCATIONS)
    m_kind = OPTINFO_KIND_SUCCESS;
  else if (dump_kind & MSG_MISSED_OPTIMIZATION)
    m_kind = OPTINFO_KIND_FAILURE;
  else if (dump_kind & MSG_NOTE)
    m_kind = OPTINFO_KIND_NOTE;
}

/* Return true if any of the active optinfo destinations make use
   of inlining information.
   (if true, then the information is preserved).  */

bool optinfo_wants_inlining_info_p ()
{
  return dump_context::get ().optimization_records_enabled_p ();
}
