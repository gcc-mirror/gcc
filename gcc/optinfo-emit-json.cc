/* Emit optimization information as JSON files.
   Copyright (C) 2018-2020 Free Software Foundation, Inc.
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
  : m_root_tuple (NULL), m_scopes ()
{
  m_root_tuple = new json::array ();

  /* Populate with metadata; compare with toplev.c: print_version.  */
  json::object *metadata = new json::object ();
  m_root_tuple->append (metadata);
  metadata->set ("format", new json::string ("1"));
  json::object *generator = new json::object ();
  metadata->set ("generator", generator);
  generator->set ("name", new json::string (lang_hooks.name));
  generator->set ("pkgversion", new json::string (pkgversion_string));
  generator->set ("version", new json::string (version_string));
  /* TARGET_NAME is passed in by the Makefile.  */
  generator->set ("target", new json::string (TARGET_NAME));

  /* TODO: capture command-line?
     see gen_producer_string in dwarf2out.c (currently static).  */

  /* TODO: capture "any plugins?" flag (or the plugins themselves).  */

  json::array *passes = new json::array ();
  m_root_tuple->append (passes);

  /* Call add_pass_list for all of the pass lists.  */
  {
#define DEF_PASS_LIST(LIST) \
    add_pass_list (passes, g->get_passes ()->LIST);
    GCC_PASS_LISTS
#undef DEF_PASS_LIST
  }

  json::array *records = new json::array ();
  m_root_tuple->append (records);

  m_scopes.safe_push (records);
}

/* optrecord_json_writer's ctor.
   Delete the in-memory JSON representation.  */

optrecord_json_writer::~optrecord_json_writer ()
{
  delete m_root_tuple;
}

/* Choose an appropriate filename, and write the saved records to it.  */

void
optrecord_json_writer::write () const
{
  pretty_printer pp;
  m_root_tuple->print (&pp);

  bool emitted_error = false;
  char *filename = concat (dump_base_name, ".opt-record.json.gz", NULL);
  gzFile outfile = gzopen (filename, "w");
  if (outfile == NULL)
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
optrecord_json_writer::add_record (const optinfo *optinfo)
{
  json::object *obj = optinfo_to_json (optinfo);

  add_record (obj);

  /* Potentially push the scope.  */
  if (optinfo->get_kind () == OPTINFO_KIND_SCOPE)
    {
      json::array *children = new json::array ();
      obj->set ("children", children);
      m_scopes.safe_push (children);
    }
}

/* Private methods of optrecord_json_writer.  */

/* Add record OBJ to the innermost scope.  */

void
optrecord_json_writer::add_record (json::object *obj)
{
  /* Add to innermost scope.  */
  gcc_assert (m_scopes.length () > 0);
  m_scopes[m_scopes.length () - 1]->append (obj);
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

json::object *
optrecord_json_writer::impl_location_to_json (dump_impl_location_t loc)
{
  json::object *obj = new json::object ();
  obj->set ("file", new json::string (loc.m_file));
  obj->set ("line", new json::integer_number (loc.m_line));
  if (loc.m_function)
    obj->set ("function", new json::string (loc.m_function));
  return obj;
}

/* Create a JSON object representing LOC.  */

json::object *
optrecord_json_writer::location_to_json (location_t loc)
{
  gcc_assert (LOCATION_LOCUS (loc) != UNKNOWN_LOCATION);
  expanded_location exploc = expand_location (loc);
  json::object *obj = new json::object ();
  obj->set ("file", new json::string (exploc.file));
  obj->set ("line", new json::integer_number (exploc.line));
  obj->set ("column", new json::integer_number (exploc.column));
  return obj;
}

/* Create a JSON object representing COUNT.  */

json::object *
optrecord_json_writer::profile_count_to_json (profile_count count)
{
  json::object *obj = new json::object ();
  obj->set ("value", new json::integer_number (count.to_gcov_type ()));
  obj->set ("quality",
	    new json::string (profile_quality_as_string (count.quality ())));
  return obj;
}

/* Get a string for use when referring to PASS in the saved optimization
   records.  */

json::string *
optrecord_json_writer::get_id_value_for_pass (opt_pass *pass)
{
  pretty_printer pp;
  /* this is host-dependent, but will be consistent for a given host.  */
  pp_pointer (&pp, static_cast<void *> (pass));
  return new json::string (pp_formatted_text (&pp));
}

/* Create a JSON object representing PASS.  */

json::object *
optrecord_json_writer::pass_to_json (opt_pass *pass)
{
  json::object *obj = new json::object ();
  const char *type = NULL;
  switch (pass->type)
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
  obj->set ("id", get_id_value_for_pass (pass));
  obj->set ("type", new json::string (type));
  obj->set ("name", new json::string (pass->name));
  /* Represent the optgroup flags as an array.  */
  {
    json::array *optgroups = new json::array ();
    obj->set ("optgroups", optgroups);
    for (const kv_pair<optgroup_flags_t> *optgroup = optgroup_options;
	 optgroup->name != NULL; optgroup++)
      if (optgroup->value != OPTGROUP_ALL
	  && (pass->optinfo_flags & optgroup->value))
	optgroups->append (new json::string (optgroup->name));
  }
  obj->set ("num", new json::integer_number (pass->static_pass_number));
  return obj;
}

/* Create a JSON array for LOC representing the chain of inlining
   locations.
   Compare with lhd_print_error_function and cp_print_error_function.  */

json::value *
optrecord_json_writer::inlining_chain_to_json (location_t loc)
{
  json::array *array = new json::array ();

  tree abstract_origin = LOCATION_BLOCK (loc);

  while (abstract_origin)
    {
      location_t *locus;
      tree block = abstract_origin;

      locus = &BLOCK_SOURCE_LOCATION (block);
      tree fndecl = NULL;
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
	  abstract_origin = NULL;
	}
      if (fndecl)
	{
	  json::object *obj = new json::object ();
	  const char *printable_name
	    = lang_hooks.decl_printable_name (fndecl, 2);
	  obj->set ("fndecl", new json::string (printable_name));
	  if (LOCATION_LOCUS (*locus) != UNKNOWN_LOCATION)
	    obj->set ("site", location_to_json (*locus));
	  array->append (obj);
	}
    }

  return array;
}

/* Create a JSON object representing OPTINFO.  */

json::object *
optrecord_json_writer::optinfo_to_json (const optinfo *optinfo)
{
  json::object *obj = new json::object ();

  obj->set ("impl_location",
	    impl_location_to_json (optinfo->get_impl_location ()));

  const char *kind_str = optinfo_kind_to_string (optinfo->get_kind ());
  obj->set ("kind", new json::string (kind_str));
  json::array *message = new json::array ();
  obj->set ("message", message);
  for (unsigned i = 0; i < optinfo->num_items (); i++)
    {
      const optinfo_item *item = optinfo->get_item (i);
      switch (item->get_kind ())
	{
	default:
	  gcc_unreachable ();
	case OPTINFO_ITEM_KIND_TEXT:
	  {
	    message->append (new json::string (item->get_text ()));
	  }
	  break;
	case OPTINFO_ITEM_KIND_TREE:
	  {
	    json::object *json_item = new json::object ();
	    json_item->set ("expr", new json::string (item->get_text ()));

	    /* Capture any location for the node.  */
	    if (LOCATION_LOCUS (item->get_location ()) != UNKNOWN_LOCATION)
	      json_item->set ("location",
			      location_to_json (item->get_location ()));

	    message->append (json_item);
	  }
	  break;
	case OPTINFO_ITEM_KIND_GIMPLE:
	  {
	    json::object *json_item = new json::object ();
	    json_item->set ("stmt", new json::string (item->get_text ()));

	    /* Capture any location for the stmt.  */
	    if (LOCATION_LOCUS (item->get_location ()) != UNKNOWN_LOCATION)
	      json_item->set ("location",
			      location_to_json (item->get_location ()));

	    message->append (json_item);
	  }
	  break;
	case OPTINFO_ITEM_KIND_SYMTAB_NODE:
	  {
	    json::object *json_item = new json::object ();
	    json_item->set ("symtab_node", new json::string (item->get_text ()));

	    /* Capture any location for the node.  */
	    if (LOCATION_LOCUS (item->get_location ()) != UNKNOWN_LOCATION)
	      json_item->set ("location",
			      location_to_json (item->get_location ()));
	    message->append (json_item);
	  }
	  break;
	}
   }

  if (optinfo->get_pass ())
    obj->set ("pass", get_id_value_for_pass (optinfo->get_pass ()));

  profile_count count = optinfo->get_count ();
  if (count.initialized_p ())
    obj->set ("count", profile_count_to_json (count));

  /* Record any location, handling the case where of an UNKNOWN_LOCATION
     within an inlined block.  */
  location_t loc = optinfo->get_location_t ();
  if (get_pure_location (line_table, loc) != UNKNOWN_LOCATION)
    {
      // TOOD: record the location (just caret for now)
      // TODO: start/finish also?
      obj->set ("location", location_to_json (loc));
    }

  if (current_function_decl)
    {
      const char *fnname
	= IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (current_function_decl));
      obj->set ("function", new json::string (fnname));
    }

  if (loc != UNKNOWN_LOCATION)
    obj->set ("inlining_chain", inlining_chain_to_json (loc));

  return obj;
}

/* Add a json description of PASS and its siblings to ARR, recursing into
   child passes (adding their descriptions within a "children" array).  */

void
optrecord_json_writer::add_pass_list (json::array *arr, opt_pass *pass)
{
  do
    {
      json::object *pass_obj = pass_to_json (pass);
      arr->append (pass_obj);
      if (pass->sub)
	{
	  json::array *sub = new json::array ();
	  pass_obj->set ("children", sub);
	  add_pass_list (sub, pass->sub);
	}
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
  ASSERT_TRUE (info != NULL);
  ASSERT_EQ (info->num_items (), 2);

  optrecord_json_writer writer;
  json::object *json_obj = writer.optinfo_to_json (info);
  ASSERT_TRUE (json_obj != NULL);

  /* Verify that the json is sane.  */
  pretty_printer pp;
  json_obj->print (&pp);
  const char *json_str = pp_formatted_text (&pp);
  ASSERT_STR_CONTAINS (json_str, "impl_location");
  ASSERT_STR_CONTAINS (json_str, "\"kind\": \"note\"");
  ASSERT_STR_CONTAINS (json_str,
		       "\"message\": [\"test of tree: \", {\"expr\": \"0\"}]");
  delete json_obj;
}

/* Run all of the selftests within this file.  */

void
optinfo_emit_json_cc_tests ()
{
  test_building_json_from_dump_calls ();
}

} // namespace selftest

#endif /* CHECKING_P */
