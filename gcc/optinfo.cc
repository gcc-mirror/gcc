/* Optimization information.
   Copyright (C) 2018 Free Software Foundation, Inc.
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

/* optinfo_item's ctor.  */

optinfo_item::optinfo_item (enum optinfo_item_kind kind, location_t location,
			    char *text, bool owned)
: m_kind (kind), m_location (location), m_text (text), m_owned (owned)
{
}

/* optinfo_item's dtor.  */

optinfo_item::~optinfo_item ()
{
  if (m_owned)
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

/* Emit the optinfo to all of the active destinations.  */

void
optinfo::emit ()
{
  /* -fsave-optimization-record.  */
  optimization_records_maybe_record_optinfo (this);
}

/* Update the optinfo's kind based on DUMP_KIND.  */

void
optinfo::handle_dump_file_kind (dump_flags_t dump_kind)
{
  if (dump_kind & MSG_OPTIMIZED_LOCATIONS)
    m_kind = OPTINFO_KIND_SUCCESS;
  else if (dump_kind & MSG_MISSED_OPTIMIZATION)
    m_kind = OPTINFO_KIND_FAILURE;
  else if (dump_kind & MSG_NOTE)
    m_kind = OPTINFO_KIND_NOTE;
}

/* Append a string literal to this optinfo.  */

void
optinfo::add_string (const char *str)
{
  optinfo_item *item
    = new optinfo_item (OPTINFO_ITEM_KIND_TEXT, UNKNOWN_LOCATION,
			const_cast <char *> (str), false);
  m_items.safe_push (item);
}

/* Append printf-formatted text to this optinfo.  */

void
optinfo::add_printf (const char *format, ...)
{
  va_list ap;
  va_start (ap, format);
  add_printf_va (format, ap);
  va_end (ap);
}

/* Append printf-formatted text to this optinfo.  */

void
optinfo::add_printf_va (const char *format, va_list ap)
{
  char *formatted_text = xvasprintf (format, ap);
  optinfo_item *item
    = new optinfo_item (OPTINFO_ITEM_KIND_TEXT, UNKNOWN_LOCATION,
			formatted_text, true);
  m_items.safe_push (item);
}

/* Append a gimple statement to this optinfo, equivalent to
   print_gimple_stmt.  */

void
optinfo::add_gimple_stmt (gimple *stmt, int spc, dump_flags_t dump_flags)
{
  pretty_printer pp;
  pp_needs_newline (&pp) = true;
  pp_gimple_stmt_1 (&pp, stmt, spc, dump_flags);
  pp_newline (&pp);

  optinfo_item *item
    = new optinfo_item (OPTINFO_ITEM_KIND_GIMPLE, gimple_location (stmt),
			xstrdup (pp_formatted_text (&pp)), true);
  m_items.safe_push (item);
}

/* Append a gimple statement to this optinfo, equivalent to
   print_gimple_expr.  */

void
optinfo::add_gimple_expr (gimple *stmt, int spc, dump_flags_t dump_flags)
{
  dump_flags |= TDF_RHS_ONLY;
  pretty_printer pp;
  pp_needs_newline (&pp) = true;
  pp_gimple_stmt_1 (&pp, stmt, spc, dump_flags);

  optinfo_item *item
    = new optinfo_item (OPTINFO_ITEM_KIND_GIMPLE, gimple_location (stmt),
			xstrdup (pp_formatted_text (&pp)), true);
  m_items.safe_push (item);
}

/* Append a tree node to this optinfo, equivalent to print_generic_expr.  */

void
optinfo::add_tree (tree node, dump_flags_t dump_flags)
{
  pretty_printer pp;
  pp_needs_newline (&pp) = true;
  pp_translate_identifiers (&pp) = false;
  dump_generic_node (&pp, node, 0, dump_flags, false);

  location_t loc = UNKNOWN_LOCATION;
  if (EXPR_HAS_LOCATION (node))
    loc = EXPR_LOCATION (node);

  optinfo_item *item
    = new optinfo_item (OPTINFO_ITEM_KIND_TREE, loc,
			xstrdup (pp_formatted_text (&pp)), true);
  m_items.safe_push (item);
}

/* Append a symbol table node to this optinfo.  */

void
optinfo::add_symtab_node (symtab_node *node)
{
  location_t loc = DECL_SOURCE_LOCATION (node->decl);
  optinfo_item *item
    = new optinfo_item (OPTINFO_ITEM_KIND_SYMTAB_NODE, loc,
			xstrdup (node->dump_name ()), true);
  m_items.safe_push (item);
}

/* Append the decimal represenation of a wide_int_ref to this
   optinfo.  */

void
optinfo::add_dec (const wide_int_ref &wi, signop sgn)
{
  char buf[WIDE_INT_PRINT_BUFFER_SIZE];
  print_dec (wi, buf, sgn);
  optinfo_item *item
    = new optinfo_item (OPTINFO_ITEM_KIND_TEXT, UNKNOWN_LOCATION,
			xstrdup (buf), true);
  m_items.safe_push (item);
}

/* Should optinfo instances be created?
   All creation of optinfos should be guarded by this predicate.
   Return true if any optinfo destinations are active.  */

bool optinfo_enabled_p ()
{
  return (dump_context::get ().forcibly_enable_optinfo_p ()
	  || optimization_records_enabled_p ());
}

/* Return true if any of the active optinfo destinations make use
   of inlining information.
   (if true, then the information is preserved).  */

bool optinfo_wants_inlining_info_p ()
{
  return optimization_records_enabled_p ();
}
