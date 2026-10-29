/* Emit optimization information as JSON files.
   Copyright (C) 2018-2026 Free Software Foundation, Inc.
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

#define INCLUDE_STRING
#include "config.h"
#include "system.h"
#include "coretypes.h"

#include "backend.h"
#include "tree.h"
#include "gimple.h"
#include "diagnostic-core.h"

#include "profile.h"
#include "output.h"
#include "tree-pass.h"

#include "optinfo.h"
#include "optinfo-emit-json.h"
#include "json.h"
#include "pretty-print.h"
#include "tree-pretty-print.h"
#include "gimple-pretty-print.h"
#include "cgraph.h"

#include "langhooks.h"
#include "version.h"
#include "context.h"
#include "pass_manager.h"
#include "selftest.h"
#include "dump-context.h"
#include <zlib.h>

/* optrecord_json_writer's ctor.  Populate the top-level parts of the
   in-memory JSON representation.  */

optrecord_json_writer::optrecord_json_writer ()
: m_root_tuple (nullptr), m_scopes ()
{
  m_root_tuple = std::make_unique<json::array> ();

  /* Populate with metadata; compare with toplev.cc: print_version.  */
  auto metadata = std::make_unique<json::object> ();
  metadata->set_string ("format", "1");
  auto generator = std::make_unique<json::object> ();
  generator->set_string ("name", lang_hooks.name);
  generator->set_string ("pkgversion", pkgversion_string);
  generator->set_string ("version", version_string);
  /* TARGET_NAME is passed in by the Makefile.  */
  generator->set_string ("target", TARGET_NAME);
  metadata->set<json::object> ("generator", std::move (generator));
  m_root_tuple->append<json::object> (std::move (metadata));

  /* TODO: capture command-line?
     see gen_producer_string in dwarf2out.cc (currently static).  */

  /* TODO: capture "any plugins?" flag (or the plugins themselves).  */

  auto passes = std::make_unique<json::array> ();

  /* Call add_pass_list for all of the pass lists.  */
  {
#define DEF_PASS_LIST(LIST) \
    add_pass_list (passes.get (), g->get_passes ()->LIST);
    GCC_PASS_LISTS
#undef DEF_PASS_LIST
  }
  m_root_tuple->append<json::array> (std::move (passes));

  auto records = std::make_unique<json::array> ();
  m_scopes.safe_push (records.get ());
  m_root_tuple->append (std::move (records));
}

/* Choose an appropriate filename, and write the saved records to it.  */

void
optrecord_json_writer::write () const
{
  pretty_printer pp;
  m_root_tuple->print (&pp, false);

  bool emitted_error = false;
  char *filename = concat (dump_base_name, ".opt-record.json.gz", nullptr);
  gzFile outfile = gzopen (filename, "w");
  if (outfile == nullptr)
    {
      error_at (UNKNOWN_LOCATION, "cannot open file %qs for writing optimization records",
		filename); // FIXME: more info?
      goto cleanup;
    }

  if (gzputs (outfile, pp_formatted_text (&pp)) <= 0)
    {
      int tmp;
      error_at (UNKNOWN_LOCATION, "error writing optimization records to %qs: %s",
		filename, gzerror (outfile, &tmp));
      emitted_error = true;
    }

 cleanup:
  if (outfile)
    if (gzclose (outfile) != Z_OK)
      if (!emitted_error)
	error_at (UNKNOWN_LOCATION, "error closing optimization records %qs",
		  filename);

  free (filename);
}

/* Add a record for OPTINFO to the queue of records to be written.  */

void
optrecord_json_writer::add_record (const optinfo &optinfo)
{
  auto obj = optinfo_to_json (optinfo);
  auto borrowed_obj = obj.get ();

  add_record (std::move (obj));

  /* Potentially push the scope.  */
  if (optinfo.get_kind () == optinfo::kind::scope)
    {
      auto children = std::make_unique<json::array> ();
      m_scopes.safe_push (children.get ());
      borrowed_obj->set<json::array> ("children", std::move (children));
    }
}

/* Private methods of optrecord_json_writer.  */

/* Add record OBJ to the innermost scope.  */

void
optrecord_json_writer::add_record (std::unique_ptr<json::object> obj)
{
  /* Add to innermost scope.  */
  gcc_assert (m_scopes.length () > 0);
  m_scopes[m_scopes.length () - 1]->append<json::object> (std::move (obj));
}

/* Pop the innermost scope.  */

void
optrecord_json_writer::pop_scope ()
{
  m_scopes.pop ();

  /* We should never pop the top-level records array.  */
  gcc_assert (m_scopes.length () > 0);
}

/* Create a JSON object representing LOC.  */

std::unique_ptr<json::object>
optrecord_json_writer::impl_location_to_json (dump_impl_location_t loc)
{
  auto obj = std::make_unique<json::object> ();
  obj->set_string ("file", loc.m_file);
  obj->set_integer ("line", loc.m_line);
  if (loc.m_function)
    obj->set_string ("function", loc.m_function);
  return obj;
}

/* Create a JSON object representing LOC.  */

std::unique_ptr<json::object>
optrecord_json_writer::location_to_json (location_t loc)
{
  gcc_assert (LOCATION_LOCUS (loc) != UNKNOWN_LOCATION);
  expanded_location exploc = expand_location (loc);
  auto obj = std::make_unique<json::object> ();
  obj->set_string ("file", exploc.file);
  obj->set_integer ("line", exploc.line);
  obj->set_integer ("column", exploc.column);
  return obj;
}

/* Create a JSON object representing COUNT.  */

std::unique_ptr<json::object>
optrecord_json_writer::profile_count_to_json (profile_count count)
{
  auto obj = std::make_unique<json::object> ();
  obj->set_integer ("value", count.to_gcov_type ());
  obj->set_string ("quality", profile_quality_as_string (count.quality ()));
  return obj;
}

/* Get a string for use when referring to PASS in the saved optimization
   records.  */

std::unique_ptr<json::string>
optrecord_json_writer::get_id_value_for_pass (const opt_pass &pass)
{
  pretty_printer pp;
  /* this is host-dependent, but will be consistent for a given host.  */
  pp_pointer (&pp, static_cast<const void *> (&pass));
  return std::make_unique<json::string> (pp_formatted_text (&pp));
}

/* Create a JSON object representing PASS.  */

std::unique_ptr<json::object>
optrecord_json_writer::pass_to_json (const opt_pass &pass)
{
  auto obj = std::make_unique<json::object> ();
  const char *type = nullptr;
  switch (pass.type)
    {
    default:
      gcc_unreachable ();
    case GIMPLE_PASS:
      type = "gimple";
      break;
    case RTL_PASS:
      type = "rtl";
      break;
    case SIMPLE_IPA_PASS:
      type = "simple_ipa";
      break;
    case IPA_PASS:
      type = "ipa";
      break;
    }
  obj->set<json::string> ("id", get_id_value_for_pass (pass));
  obj->set_string ("type", type);
  obj->set_string ("name", pass.name);
  /* Represent the optgroup flags as an array.  */
  {
    auto optgroups = std::make_unique<json::array> ();
    for (const kv_pair<optgroup_flags_t> *optgroup = optgroup_options;
	 optgroup->name != nullptr; optgroup++)
      if (optgroup->value != OPTGROUP_ALL
	  && (pass.optinfo_flags & optgroup->value))
	optgroups->append_string (optgroup->name);
    obj->set<json::array> ("optgroups", std::move (optgroups));
  }
  obj->set_integer ("num", pass.static_pass_number);
  return obj;
}

/* Create a JSON array for LOC representing the chain of inlining
   locations.
   Compare with lhd_print_error_function and cp_print_error_function.  */

std::unique_ptr<json::array>
optrecord_json_writer::inlining_chain_to_json (location_t loc)
{
  auto array = std::make_unique<json::array> ();

  tree abstract_origin = LOCATION_BLOCK (loc);

  while (abstract_origin)
    {
      location_t *locus;
      tree block = abstract_origin;

      locus = &BLOCK_SOURCE_LOCATION (block);
      tree fndecl = nullptr;
      block = BLOCK_SUPERCONTEXT (block);
      while (block && TREE_CODE (block) == BLOCK
	     && BLOCK_ABSTRACT_ORIGIN (block))
	{
	  tree ao = BLOCK_ABSTRACT_ORIGIN (block);
	  if (TREE_CODE (ao) == FUNCTION_DECL)
	    {
	      fndecl = ao;
	      break;
	    }
	  else if (TREE_CODE (ao) != BLOCK)
	    break;

	  block = BLOCK_SUPERCONTEXT (block);
	}
      if (fndecl)
	abstract_origin = block;
      else
	{
	  while (block && TREE_CODE (block) == BLOCK)
	    block = BLOCK_SUPERCONTEXT (block);

	  if (block && TREE_CODE (block) == FUNCTION_DECL)
	    fndecl = block;
	  abstract_origin = nullptr;
	}
      if (fndecl)
	{
	  auto obj = std::make_unique<json::object> ();
	  const char *printable_name
	    = lang_hooks.decl_printable_name (fndecl, 2);
	  obj->set_string ("fndecl", printable_name);
	  if (LOCATION_LOCUS (*locus) != UNKNOWN_LOCATION)
	    obj->set<json::object> ("site", location_to_json (*locus));
	  array->append<json::object> (std::move (obj));
	}
    }

  return array;
}

/* Create a JSON object representing OPTINFO.  */

std::unique_ptr<json::object>
optrecord_json_writer::optinfo_to_json (const optinfo &optinfo)
{
  auto obj = std::make_unique<json::object> ();

  obj->set<json::object>
    ("impl_location",
     impl_location_to_json (optinfo.get_impl_location ()));

  const char *kind_str = optinfo::kind_to_string (optinfo.get_kind ());
  obj->set_string ("kind", kind_str);
  auto message = std::make_unique<json::array> ();
  for (unsigned i = 0; i < optinfo.num_items (); i++)
    {
      const optinfo_item *item = optinfo.get_item (i);
      switch (item->get_kind ())
	{
	default:
	  gcc_unreachable ();
	case optinfo_item::kind::text:
	  {
	    message->append_string (item->get_text ());
	  }
	  break;
	case optinfo_item::kind::tree:
	  {
	    auto json_item = std::make_unique<json::object> ();
	    json_item->set_string ("expr", item->get_text ());

	    /* Capture any location for the node.  */
	    if (LOCATION_LOCUS (item->get_location ()) != UNKNOWN_LOCATION)
	      json_item->set<json::object>
		("location",
		 location_to_json (item->get_location ()));

	    message->append<json::object> (std::move (json_item));
	  }
	  break;
	case optinfo_item::kind::gimple:
	  {
	    auto json_item = std::make_unique<json::object> ();
	    json_item->set_string ("stmt", item->get_text ());

	    /* Capture any location for the stmt.  */
	    if (LOCATION_LOCUS (item->get_location ()) != UNKNOWN_LOCATION)
	      json_item->set<json::object>
		("location",
		 location_to_json (item->get_location ()));

	    message->append<json::object> (std::move (json_item));
	  }
	  break;
	case optinfo_item::kind::symtab_node:
	  {
	    auto json_item = std::make_unique<json::object> ();
	    json_item->set_string ("symtab_node", item->get_text ());

	    /* Capture any location for the node.  */
	    if (LOCATION_LOCUS (item->get_location ()) != UNKNOWN_LOCATION)
	      json_item->set<json::object>
		("location",
		 location_to_json (item->get_location ()));
	    message->append<json::object> (std::move (json_item));
	  }
	  break;
	}
   }
  obj->set<json::array> ("message", std::move (message));

  if (auto pass = optinfo.get_pass ())
    obj->set<json::string> ("pass",
			    get_id_value_for_pass (*pass));

  profile_count count = optinfo.get_count ();
  if (count.initialized_p ())
    obj->set<json::object> ("count", profile_count_to_json (count));

  /* Record any location, handling the case where of an UNKNOWN_LOCATION
     within an inlined block.  */
  location_t loc = optinfo.get_location_t ();
  if (get_pure_location (line_table, loc) != UNKNOWN_LOCATION)
    {
      // TOOD: record the location (just caret for now)
      // TODO: start/finish also?
      obj->set<json::object> ("location", location_to_json (loc));
    }

  if (current_function_decl)
    {
      const char *fnname
	= IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (current_function_decl));
      obj->set_string ("function", fnname);
    }

  if (loc != UNKNOWN_LOCATION)
    obj->set<json::array> ("inlining_chain", inlining_chain_to_json (loc));

  return obj;
}

/* Add a json description of PASS and its siblings to ARR, recursing into
   child passes (adding their descriptions within a "children" array).  */

void
optrecord_json_writer::add_pass_list (json::array *arr,
				      const opt_pass *pass)
{
  gcc_assert (pass);
  do
    {
      auto pass_obj = pass_to_json (*pass);
      if (pass->sub)
	{
	  auto child_arr = std::make_unique<json::array> ();
	  add_pass_list (child_arr.get (), pass->sub);
	  pass_obj->set ("children", std::move (child_arr));
	}
      arr->append (std::move (pass_obj));
      pass = pass->next;
    }
  while (pass);
}

#if CHECKING_P

namespace selftest {

/* Verify that we can build a JSON optimization record from dump_*
   calls.  */

static void
test_building_json_from_dump_calls ()
{
  temp_dump_context tmp (true, true, MSG_NOTE);
  dump_user_location_t loc;
  dump_printf_loc (MSG_NOTE, loc, "test of tree: ");
  dump_generic_expr (MSG_NOTE, TDF_SLIM, integer_zero_node);
  optinfo *info = tmp.get_pending_optinfo ();
  ASSERT_TRUE (info != nullptr);
  ASSERT_EQ (info->num_items (), 2);

  optrecord_json_writer writer;
  auto json_obj = writer.optinfo_to_json (*info);
  ASSERT_TRUE (json_obj != nullptr);

  /* Verify that the json is sane.  */
  pretty_printer pp;
  json_obj->print (&pp, false);
  const char *json_str = pp_formatted_text (&pp);
  ASSERT_STR_CONTAINS (json_str, "impl_location");
  ASSERT_STR_CONTAINS (json_str, "\"kind\": \"note\"");
  ASSERT_STR_CONTAINS (json_str,
		       "\"message\": [\"test of tree: \", {\"expr\": \"0\"}]");
}

/* Run all of the selftests within this file.  */

void
optinfo_emit_json_cc_tests ()
{
  test_building_json_from_dump_calls ();
}

} // namespace selftest

#endif /* CHECKING_P */
