/* Dump infrastructure for optimizations and intermediate representation.
   Copyright (C) 2012-2025 Free Software Foundation, Inc.

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
#include "options.h"
#include "tree.h"
#include "gimple-pretty-print.h"
#include "diagnostic-core.h"
#include "dumpfile.h"
#include "context.h"
#include "profile-count.h"
#include "tree-cfg.h"
#include "langhooks.h"
#include "backend.h" /* for gimple.h.  */
#include "gimple.h" /* for dump_user_location_t ctor.  */
#include "rtl.h" /* for dump_user_location_t ctor.  */
#include "selftest.h"
#include "optinfo.h"
#include "dump-context.h"
#include "cgraph.h"
#include "tree-pass.h" /* for "current_pass".  */
#include "optinfo-emit-json.h"
#include "stringpool.h" /* for get_identifier.  */
#include "spellcheck.h"
#include "pretty-print-format-impl.h"

/* If non-NULL, return one past-the-end of the matching SUBPART of
   the WHOLE string.  */
#define skip_leading_substring(whole,  part) \
   (strncmp (whole, part, strlen (part)) ? NULL : whole + strlen (part))

static dump_flags_t pflags;		      /* current dump_flags */

static void dump_loc (dump_flags_t, FILE *, location_t);

/* Current -fopt-info output stream, if any, and flags.  */
static FILE *alt_dump_file = NULL;
static dump_flags_t alt_flags;

static FILE *dump_open_alternate_stream (struct dump_file_info *);

/* These are currently used for communicating between passes.
   However, instead of accessing them directly, the passes can use
   dump_printf () for dumps.  */
FILE *dump_file = NULL;
const char *dump_file_name;
dump_flags_t dump_flags;
bool dumps_are_enabled = false;


/* Set global "dump_file" to NEW_DUMP_FILE, refreshing the "dumps_are_enabled"
   global.  */

void
set_dump_file (FILE *new_dump_file)
{
  dumpfile_ensure_any_optinfo_are_flushed ();
  dump_file = new_dump_file;
  dump_context::get ().refresh_dumps_are_enabled ();
}

/* Set "alt_dump_file" to NEW_ALT_DUMP_FILE, refreshing the "dumps_are_enabled"
   global.  */

static void
set_alt_dump_file (FILE *new_alt_dump_file)
{
  dumpfile_ensure_any_optinfo_are_flushed ();
  alt_dump_file = new_alt_dump_file;
  dump_context::get ().refresh_dumps_are_enabled ();
}

#define DUMP_FILE_INFO(suffix, swtch, dkind, num) \
  {suffix, swtch, NULL, NULL, NULL, NULL, NULL, dkind, TDF_NONE, TDF_NONE, \
   OPTGROUP_NONE, 0, 0, num, false, false}

/* Table of tree dump switches. This must be consistent with the
   TREE_DUMP_INDEX enumeration in dumpfile.h.  */
static struct dump_file_info dump_files[TDI_end] =
{
  DUMP_FILE_INFO (NULL, NULL, DK_none, 0),
  DUMP_FILE_INFO (".cgraph", "ipa-cgraph", DK_ipa, 0),
  DUMP_FILE_INFO (".type-inheritance", "ipa-type-inheritance", DK_ipa, 0),
  DUMP_FILE_INFO (".ipa-clones", "ipa-clones", DK_ipa, 0),
  DUMP_FILE_INFO (".original", "tree-original", DK_tree, 0),
  DUMP_FILE_INFO (".gimple", "tree-gimple", DK_tree, 0),
  DUMP_FILE_INFO (".nested", "tree-nested", DK_tree, 0),
  DUMP_FILE_INFO (".lto-stream-out", "ipa-lto-stream-out", DK_ipa, 0),
  DUMP_FILE_INFO (".profile-report", "profile-report", DK_ipa, 0),
#define FIRST_AUTO_NUMBERED_DUMP 1
#define FIRST_ME_AUTO_NUMBERED_DUMP 6

  DUMP_FILE_INFO (NULL, "lang-all", DK_lang, 0),
  DUMP_FILE_INFO (NULL, "tree-all", DK_tree, 0),
  DUMP_FILE_INFO (NULL, "rtl-all", DK_rtl, 0),
  DUMP_FILE_INFO (NULL, "ipa-all", DK_ipa, 0),
};

/* Table of dump options. This must be consistent with the TDF_* flags
   in dumpfile.h and opt_info_options below. */
static const kv_pair<dump_flags_t> dump_options[] =
{
  {"none", TDF_NONE},
  {"address", TDF_ADDRESS},
  {"asmname", TDF_ASMNAME},
  {"slim", TDF_SLIM},
  {"raw", TDF_RAW},
  {"graph", TDF_GRAPH},
  {"details", (TDF_DETAILS | MSG_OPTIMIZED_LOCATIONS
               | MSG_MISSED_OPTIMIZATION
               | MSG_NOTE)},
  {"cselib", TDF_CSELIB},
  {"stats", TDF_STATS},
  {"blocks", TDF_BLOCKS},
  {"vops", TDF_VOPS},
  {"lineno", TDF_LINENO},
  {"uid", TDF_UID},
  {"stmtaddr", TDF_STMTADDR},
  {"memsyms", TDF_MEMSYMS},
  {"eh", TDF_EH},
  {"alias", TDF_ALIAS},
  {"nouid", TDF_NOUID},
  {"enumerate_locals", TDF_ENUMERATE_LOCALS},
  {"scev", TDF_SCEV},
  {"gimple", TDF_GIMPLE},
  {"folding", TDF_FOLDING},
  {"optimized", MSG_OPTIMIZED_LOCATIONS},
  {"missed", MSG_MISSED_OPTIMIZATION},
  {"note", MSG_NOTE},
  {"optall", MSG_ALL_KINDS},
  {"all", dump_flags_t (TDF_ALL_VALUES
			& ~(TDF_RAW | TDF_SLIM | TDF_LINENO | TDF_GRAPH
			    | TDF_STMTADDR | TDF_RHS_ONLY | TDF_NOUID
			    | TDF_ENUMERATE_LOCALS | TDF_SCEV | TDF_GIMPLE))},
  {NULL, TDF_NONE}
};

/* A subset of the dump_options table which is used for -fopt-info
   types. This must be consistent with the MSG_* flags in dumpfile.h.
 */
static const kv_pair<dump_flags_t> optinfo_verbosity_options[] =
{
  {"optimized", MSG_OPTIMIZED_LOCATIONS},
  {"missed", MSG_MISSED_OPTIMIZATION},
  {"note", MSG_NOTE},
  {"all", MSG_ALL_KINDS},
  {"internals", MSG_PRIORITY_INTERNALS},
  {NULL, TDF_NONE}
};

/* Flags used for -fopt-info groups.  */
const kv_pair<optgroup_flags_t> optgroup_options[] =
{
  {"ipa", OPTGROUP_IPA},
  {"loop", OPTGROUP_LOOP},
  {"inline", OPTGROUP_INLINE},
  {"omp", OPTGROUP_OMP},
  {"vec", OPTGROUP_VEC},
  {"optall", OPTGROUP_ALL},
  {NULL, OPTGROUP_NONE}
};

gcc::dump_manager::dump_manager ():
  m_next_dump (FIRST_AUTO_NUMBERED_DUMP),
  m_extra_dump_files (NULL),
  m_extra_dump_files_in_use (0),
  m_extra_dump_files_alloced (0),
  m_optgroup_flags (OPTGROUP_NONE),
  m_optinfo_flags (TDF_NONE),
  m_optinfo_filename (NULL)
{
}

gcc::dump_manager::~dump_manager ()
{
  free (m_optinfo_filename);
  for (size_t i = 0; i < m_extra_dump_files_in_use; i++)
    {
      dump_file_info *dfi = &m_extra_dump_files[i];
      /* suffix, swtch, glob are statically allocated for the entries
	 in dump_files, and for statistics, but are dynamically allocated
	 for those for passes.  */
      if (dfi->owns_strings)
	{
	  XDELETEVEC (const_cast <char *> (dfi->suffix));
	  XDELETEVEC (const_cast <char *> (dfi->swtch));
	  XDELETEVEC (const_cast <char *> (dfi->glob));
	}
      /* These, if non-NULL, are always dynamically allocated.  */
      XDELETEVEC (const_cast <char *> (dfi->pfilename));
      XDELETEVEC (const_cast <char *> (dfi->alt_filename));
    }
  XDELETEVEC (m_extra_dump_files);
}

unsigned int
gcc::dump_manager::
dump_register (const char *suffix, const char *swtch, const char *glob,
	       dump_kind dkind, optgroup_flags_t optgroup_flags,
	       bool take_ownership)
{
  int num = m_next_dump++;

  size_t count = m_extra_dump_files_in_use++;

  if (count >= m_extra_dump_files_alloced)
    {
      if (m_extra_dump_files_alloced == 0)
	m_extra_dump_files_alloced = 512;
      else
	m_extra_dump_files_alloced *= 2;
      m_extra_dump_files = XRESIZEVEC (struct dump_file_info,
				       m_extra_dump_files,
				       m_extra_dump_files_alloced);

      /* Construct a new object in the space allocated above.  */
      new (m_extra_dump_files + count) dump_file_info ();
    }
  else
    {
      /* Zero out the already constructed object.  */
      m_extra_dump_files[count] = dump_file_info ();
    }

  m_extra_dump_files[count].suffix = suffix;
  m_extra_dump_files[count].swtch = swtch;
  m_extra_dump_files[count].glob = glob;
  m_extra_dump_files[count].dkind = dkind;
  m_extra_dump_files[count].optgroup_flags = optgroup_flags;
  m_extra_dump_files[count].num = num;
  m_extra_dump_files[count].owns_strings = take_ownership;

  return count + TDI_end;
}


/* Allow languages and middle-end to register their dumps before the
   optimization passes.  */

void
gcc::dump_manager::
register_dumps ()
{
  lang_hooks.register_dumps (this);
  /* If this assert fails, some FE registered more than
     FIRST_ME_AUTO_NUMBERED_DUMP - FIRST_AUTO_NUMBERED_DUMP
     dump files.  Bump FIRST_ME_AUTO_NUMBERED_DUMP accordingly.  */
  gcc_assert (m_next_dump <= FIRST_ME_AUTO_NUMBERED_DUMP);
  m_next_dump = FIRST_ME_AUTO_NUMBERED_DUMP;
  dump_files[TDI_original].num = m_next_dump++;
  dump_files[TDI_gimple].num = m_next_dump++;
  dump_files[TDI_nested].num = m_next_dump++;
}


/* Return the dump_file_info for the given phase.  */

struct dump_file_info *
gcc::dump_manager::
get_dump_file_info (int phase) const
{
  if (phase < TDI_end)
    return &dump_files[phase];
  else if ((size_t) (phase - TDI_end) >= m_extra_dump_files_in_use)
    return NULL;
  else
    return m_extra_dump_files + (phase - TDI_end);
}

/* Locate the dump_file_info with swtch equal to SWTCH,
   or return NULL if no such dump_file_info exists.  */

struct dump_file_info *
gcc::dump_manager::
get_dump_file_info_by_switch (const char *swtch) const
{
  for (unsigned i = 0; i < m_extra_dump_files_in_use; i++)
    if (strcmp (m_extra_dump_files[i].swtch, swtch) == 0)
      return &m_extra_dump_files[i];

  /* Not found.  */
  return NULL;
}


/* Return the name of the dump file for the given phase.
   The caller is responsible for calling free on the returned
   buffer.
   If the dump is not enabled, returns NULL.  */

char *
gcc::dump_manager::
get_dump_file_name (int phase, int part) const
{
  struct dump_file_info *dfi;

  if (phase == TDI_none)
    return NULL;

  dfi = get_dump_file_info (phase);

  return get_dump_file_name (dfi, part);
}

/* Return the name of the dump file for the given dump_file_info.
   The caller is responsible for calling free on the returned
   buffer.
   If the dump is not enabled, returns NULL.  */

char *
gcc::dump_manager::
get_dump_file_name (struct dump_file_info *dfi, int part) const
{
  char dump_id[10];

  gcc_assert (dfi);

  if (dfi->pstate == 0)
    return NULL;

  /* If available, use the command line dump filename. */
  if (dfi->pfilename)
    return xstrdup (dfi->pfilename);

  if (dfi->num < 0)
    dump_id[0] = '\0';
  else
    {
      /* (null), LANG, TREE, RTL, IPA.  */
      char suffix = " ltri"[dfi->dkind];

      if (snprintf (dump_id, sizeof (dump_id), ".%03d%c", dfi->num, suffix) < 0)
	dump_id[0] = '\0';
    }

  if (part != -1)
    {
       char part_id[8];
       snprintf (part_id, sizeof (part_id), ".%i", part);
       return concat (dump_base_name, dump_id, part_id, dfi->suffix, NULL);
    }
  else
    return concat (dump_base_name, dump_id, dfi->suffix, NULL);
}

/* Open a dump file called FILENAME.  Some filenames are special and
   refer to the standard streams.  TRUNC indicates whether this is the
   first open (so the file should be truncated, rather than appended).
   An error message is emitted in the event of failure.  */

static FILE *
dump_open (const char *filename, bool trunc)
{
  if (strcmp ("stderr", filename) == 0)
    return stderr;

  if (strcmp ("stdout", filename) == 0
      || strcmp ("-", filename) == 0)
    return stdout;

  FILE *stream = fopen (filename, trunc ? "w" : "a");

  if (!stream)
    error ("could not open dump file %qs: %m", filename);
  return stream;
}

/* For a given DFI, open an alternate dump filename (which could also
   be a standard stream such as stdout/stderr). If the alternate dump
   file cannot be opened, return NULL.  */

static FILE *
dump_open_alternate_stream (struct dump_file_info *dfi)
{
  if (!dfi->alt_filename)
    return NULL;

  if (dfi->alt_stream)
    return dfi->alt_stream;

  FILE *stream = dump_open (dfi->alt_filename, dfi->alt_state < 0);

  if (stream)
    dfi->alt_state = 1;

  return stream;
}

/* Construct a dump_user_location_t from STMT (using its location and
   hotness).  */

dump_user_location_t::dump_user_location_t (const gimple *stmt)
: m_count (), m_loc (UNKNOWN_LOCATION)
{
  if (stmt)
    {
      if (stmt->bb)
	m_count = stmt->bb->count;
      m_loc = gimple_location (stmt);
    }
}

/* Construct a dump_user_location_t from an RTL instruction (using its
   location and hotness).  */

dump_user_location_t::dump_user_location_t (const rtx_insn *insn)
: m_count (), m_loc (UNKNOWN_LOCATION)
{
  if (insn)
    {
      basic_block bb = BLOCK_FOR_INSN (insn);
      if (bb)
	m_count = bb->count;
      m_loc = INSN_LOCATION (insn);
    }
}

/* Construct from a function declaration.  This one requires spelling out
   to avoid accidentally constructing from other kinds of tree.  */

dump_user_location_t
dump_user_location_t::from_function_decl (tree fndecl)
{
  gcc_assert (fndecl);

  // FIXME: profile count for function?
  return dump_user_location_t (profile_count (),
			       DECL_SOURCE_LOCATION (fndecl));
}

/* Extract the MSG_* component from DUMP_KIND and return a string for use
   as a prefix to dump messages.
   These match the strings in optinfo_verbosity_options and thus the
   "OPTIONS" within "-fopt-info-OPTIONS".  */

static const char *
kind_as_string (dump_flags_t dump_kind)
{
  switch (dump_kind & MSG_ALL_KINDS)
    {
    default:
      gcc_unreachable ();
    case MSG_OPTIMIZED_LOCATIONS:
      return "optimized";
    case MSG_MISSED_OPTIMIZATION:
      return "missed";
    case MSG_NOTE:
      return "note";
    }
}

/* Print source location on DFILE if enabled.  */

static void
dump_loc (dump_flags_t dump_kind, FILE *dfile, location_t loc)
{
  if (dump_kind)
    {
      if (LOCATION_LOCUS (loc) > BUILTINS_LOCATION)
        fprintf (dfile, "%s:%d:%d: ", LOCATION_FILE (loc),
                 LOCATION_LINE (loc), LOCATION_COLUMN (loc));
      else if (current_function_decl)
        fprintf (dfile, "%s:%d:%d: ",
                 DECL_SOURCE_FILE (current_function_decl),
                 DECL_SOURCE_LINE (current_function_decl),
                 DECL_SOURCE_COLUMN (current_function_decl));
      fprintf (dfile, "%s: ", kind_as_string (dump_kind));
      /* Indentation based on scope depth.  */
      fprintf (dfile, "%*s", get_dump_scope_depth (), "");
    }
}

/* Print source location to PP if enabled.  */

static void
dump_loc (dump_flags_t dump_kind, pretty_printer *pp, location_t loc)
{
  /* Disable warnings about missing quoting in GCC diagnostics for
     the pp_printf calls.  Their format strings aren't used to format
     diagnostics so don't need to follow GCC diagnostic conventions.  */
#if __GNUC__ >= 10
#  pragma GCC diagnostic push
#  pragma GCC diagnostic ignored "-Wformat-diag"
#endif

  if (dump_kind)
    {
      if (LOCATION_LOCUS (loc) > BUILTINS_LOCATION)
	pp_printf (pp, "%s:%d:%d: ", LOCATION_FILE (loc),
		   LOCATION_LINE (loc), LOCATION_COLUMN (loc));
      else if (current_function_decl)
	pp_printf (pp, "%s:%d:%d: ",
		   DECL_SOURCE_FILE (current_function_decl),
		   DECL_SOURCE_LINE (current_function_decl),
		   DECL_SOURCE_COLUMN (current_function_decl));
      pp_printf (pp, "%s: ", kind_as_string (dump_kind));
      /* Indentation based on scope depth.  */
      for (unsigned i = 0; i < get_dump_scope_depth (); i++)
	pp_character (pp, ' ');
    }

#if __GNUC__ >= 10
#  pragma GCC diagnostic pop
#endif
}

/* Implementation of dump_context member functions.  */

/* dump_context's dtor.  */

dump_context::~dump_context ()
{
  delete m_pending;
}

void
dump_context::set_json_writer (optrecord_json_writer *writer)
{
  delete m_json_writer;
  m_json_writer = writer;
}

/* Perform cleanup activity for -fsave-optimization-record.
   Currently, the file is written out here in one go, before cleaning
   up.  */

void
dump_context::finish_any_json_writer ()
{
  if (!m_json_writer)
    return;

  m_json_writer->write ();
  delete m_json_writer;
  m_json_writer = NULL;
}

/* Update the "dumps_are_enabled" global; to be called whenever dump_file
   or alt_dump_file change, or when changing dump_context in selftests.  */

void
dump_context::refresh_dumps_are_enabled ()
{
  dumps_are_enabled = (dump_file || alt_dump_file || optinfo_enabled_p ()
		       || m_test_pp);
}

/* Determine if a message of kind DUMP_KIND and at the current scope depth
   should be printed.

   Only show messages that match FILTER both on their kind *and*
   their priority.  */

bool
dump_context::apply_dump_filter_p (dump_flags_t dump_kind,
				   dump_flags_t filter) const
{
  /* Few messages, if any, have an explicit MSG_PRIORITY.
     If DUMP_KIND does, we'll use it.
     Otherwise, generate an implicit priority value for the message based
     on the current scope depth.
     Messages at the top-level scope are MSG_PRIORITY_USER_FACING,
     whereas those in nested scopes are MSG_PRIORITY_INTERNALS.  */
  if (!(dump_kind & MSG_ALL_PRIORITIES))
    {
      dump_flags_t implicit_priority
	=  (m_scope_depth > 0
	    ? MSG_PRIORITY_INTERNALS
	    : MSG_PRIORITY_USER_FACING);
      dump_kind |= implicit_priority;
    }

  return (dump_kind & (filter & MSG_ALL_KINDS)
	  && dump_kind & (filter & MSG_ALL_PRIORITIES));
}

/* Print LOC to the appropriate dump destinations, given DUMP_KIND.
   If optinfos are enabled, begin a new optinfo.  */

void
dump_context::dump_loc (const dump_metadata_t &metadata,
			const dump_user_location_t &loc)
{
  end_any_optinfo ();

  dump_loc_immediate (metadata.get_dump_flags (), loc);

  if (optinfo_enabled_p ())
    begin_next_optinfo (metadata, loc);
}

/* As dump_loc above, but without starting a new optinfo. */

void
dump_context::dump_loc_immediate (dump_flags_t dump_kind,
				  const dump_user_location_t &loc)
{
  location_t srcloc = loc.get_location_t ();

  if (dump_file && apply_dump_filter_p (dump_kind, pflags))
    ::dump_loc (dump_kind, dump_file, srcloc);

  if (alt_dump_file && apply_dump_filter_p (dump_kind, alt_flags))
    ::dump_loc (dump_kind, alt_dump_file, srcloc);

  /* Support for temp_dump_context in selftests.  */
  if (m_test_pp && apply_dump_filter_p (dump_kind, m_test_pp_flags))
    ::dump_loc (dump_kind, m_test_pp, srcloc);
}

/* Make an item for the given dump call, equivalent to print_gimple_stmt.  */

static std::unique_ptr<optinfo_item>
make_item_for_dump_gimple_stmt (gimple *stmt, int spc, dump_flags_t dump_flags)
{
  pretty_printer pp;
  pp_needs_newline (&pp) = true;
  pp_gimple_stmt_1 (&pp, stmt, spc, dump_flags);
  pp_newline (&pp);

  std::unique_ptr<optinfo_item> item
    = std::make_unique<optinfo_item> (OPTINFO_ITEM_KIND_GIMPLE,
				      gimple_location (stmt),
				      xstrdup (pp_formatted_text (&pp)));
  return item;
}

/* Dump gimple statement GS with SPC indentation spaces and
   EXTRA_DUMP_FLAGS on the dump streams if DUMP_KIND is enabled.  */

void
dump_context::dump_gimple_stmt (const dump_metadata_t &metadata,
				dump_flags_t extra_dump_flags,
				gimple *gs, int spc)
{
  auto item
    = make_item_for_dump_gimple_stmt (gs, spc, dump_flags | extra_dump_flags);
  emit_item (*item.get (), metadata.get_dump_flags ());

  if (optinfo_enabled_p ())
    {
      optinfo &info = ensure_pending_optinfo (metadata);
      info.add_item (std::move (item));
    }
}

/* Similar to dump_gimple_stmt, except additionally print source location.  */

void
dump_context::dump_gimple_stmt_loc (const dump_metadata_t &metadata,
				    const dump_user_location_t &loc,
				    dump_flags_t extra_dump_flags,
				    gimple *gs, int spc)
{
  dump_loc (metadata, loc);
  dump_gimple_stmt (metadata, extra_dump_flags, gs, spc);
}

/* Make an item for the given dump call, equivalent to print_gimple_expr.  */

static std::unique_ptr<optinfo_item>
make_item_for_dump_gimple_expr (gimple *stmt, int spc, dump_flags_t dump_flags)
{
  dump_flags |= TDF_RHS_ONLY;
  pretty_printer pp;
  pp_needs_newline (&pp) = true;
  pp_gimple_stmt_1 (&pp, stmt, spc, dump_flags);

  std::unique_ptr<optinfo_item> item
    = std::make_unique<optinfo_item> (OPTINFO_ITEM_KIND_GIMPLE,
				      gimple_location (stmt),
				      xstrdup (pp_formatted_text (&pp)));
  return item;
}

/* Dump gimple statement GS with SPC indentation spaces and
   EXTRA_DUMP_FLAGS on the dump streams if DUMP_KIND is enabled.
   Do not terminate with a newline or semicolon.  */

void
dump_context::dump_gimple_expr (const dump_metadata_t &metadata,
				dump_flags_t extra_dump_flags,
				gimple *gs, int spc)
{
  std::unique_ptr<optinfo_item> item
    = make_item_for_dump_gimple_expr (gs, spc, dump_flags | extra_dump_flags);
  emit_item (*item.get (), metadata.get_dump_flags ());

  if (optinfo_enabled_p ())
    {
      optinfo &info = ensure_pending_optinfo (metadata);
      info.add_item (std::move (item));
    }
}

/* Similar to dump_gimple_expr, except additionally print source location.  */

void
dump_context::dump_gimple_expr_loc (const dump_metadata_t &metadata,
				    const dump_user_location_t &loc,
				    dump_flags_t extra_dump_flags,
				    gimple *gs,
				    int spc)
{
  dump_loc (metadata, loc);
  dump_gimple_expr (metadata, extra_dump_flags, gs, spc);
}

/* Make an item for the given dump call, equivalent to print_generic_expr.  */

static std::unique_ptr<optinfo_item>
make_item_for_dump_generic_expr (tree node, dump_flags_t dump_flags)
{
  pretty_printer pp;
  pp_needs_newline (&pp) = true;
  pp_translate_identifiers (&pp) = false;
  dump_generic_node (&pp, node, 0, dump_flags, false);

  location_t loc = UNKNOWN_LOCATION;
  if (EXPR_HAS_LOCATION (node))
    loc = EXPR_LOCATION (node);

  std::unique_ptr<optinfo_item> item
    = std::make_unique<optinfo_item> (OPTINFO_ITEM_KIND_TREE, loc,
				      xstrdup (pp_formatted_text (&pp)));
  return item;
}

/* Dump expression tree T using EXTRA_DUMP_FLAGS on dump streams if
   DUMP_KIND is enabled.  */

void
dump_context::dump_generic_expr (const dump_metadata_t &metadata,
				 dump_flags_t extra_dump_flags,
				 tree t)
{
  std::unique_ptr<optinfo_item> item
    = make_item_for_dump_generic_expr (t, dump_flags | extra_dump_flags);
  emit_item (*item.get (), metadata.get_dump_flags ());

  if (optinfo_enabled_p ())
    {
      optinfo &info = ensure_pending_optinfo (metadata);
      info.add_item (std::move (item));
    }
}


/* Similar to dump_generic_expr, except additionally print the source
   location.  */

void
dump_context::dump_generic_expr_loc (const dump_metadata_t &metadata,
				     const dump_user_location_t &loc,
				     dump_flags_t extra_dump_flags,
				     tree t)
{
  dump_loc (metadata, loc);
  dump_generic_expr (metadata, extra_dump_flags, t);
}

/* Make an item for the given dump call.  */

static std::unique_ptr<optinfo_item>
make_item_for_dump_symtab_node (symtab_node *node)
{
  location_t loc = DECL_SOURCE_LOCATION (node->decl);
  std::unique_ptr<optinfo_item> item
    = std::make_unique<optinfo_item> (OPTINFO_ITEM_KIND_SYMTAB_NODE, loc,
				      xstrdup (node->dump_name ()));
  return item;
}

struct wrapped_optinfo_item : public pp_token_custom_data::value
{
  wrapped_optinfo_item (std::unique_ptr<optinfo_item> item)
  : m_optinfo_item (std::move (item))
  {
    gcc_assert (m_optinfo_item.get ());
  }

  void dump (FILE *out) const final override
  {
    fprintf (out, "OPTINFO(\"%s\")", m_optinfo_item->get_text ());
  }

  bool as_standard_tokens (pp_token_list &) final override
  {
    /* Keep as a custom token.  */
    return false;
  }

  std::unique_ptr<optinfo_item> m_optinfo_item;
};

/* dump_pretty_printer's ctor.  */

dump_pretty_printer::dump_pretty_printer (dump_context *context,
					  dump_flags_t dump_kind)
: pretty_printer (),
  m_context (context),
  m_dump_kind (dump_kind),
  m_token_printer (*this)
{
  pp_format_decoder (this) = format_decoder_cb;
  set_token_printer (&m_token_printer);
}

/* Emit ITEM and take ownership of it.  If DEST is non-NULL, add ITEM
   to DEST; otherwise delete ITEM.  */

void
dump_pretty_printer::emit_item (std::unique_ptr<optinfo_item> item,
				optinfo *dest)
{
  m_context->emit_item (*item.get (), m_dump_kind);
  if (dest)
    dest->add_item (std::move (item));
}

/* Append a custom pp_token for ITEM (generated in phase 2 of formatting)
   into FORMATTTED_TOK_LIST, so that it can be emitted in phase 2.  */

void
dump_pretty_printer::stash_item (pp_token_list &formatted_tok_list,
				 std::unique_ptr<optinfo_item> item)
{
  gcc_assert (item.get ());

  auto custom_data
    = std::make_unique<wrapped_optinfo_item> (std::move (item));
  formatted_tok_list.push_back<pp_token_custom_data> (std::move (custom_data));
}

/* pp_format_decoder callback for dump_pretty_printer, and thus for
   dump_printf and dump_printf_loc.

   A wrapper around decode_format, for type-safety.  */

bool
dump_pretty_printer::format_decoder_cb (pretty_printer *pp, text_info *text,
					const char *spec, int /*precision*/,
					bool /*wide*/, bool /*set_locus*/,
					bool /*verbose*/, bool */*quoted*/,
					pp_token_list &formatted_tok_list)
{
  dump_pretty_printer *opp = static_cast <dump_pretty_printer *> (pp);
  return opp->decode_format (text, spec, formatted_tok_list);
}

/* Format decoder for dump_pretty_printer, and thus for dump_printf and
   dump_printf_loc.

   Supported format codes (in addition to the standard pretty_printer ones)
   are:

   %C: cgraph_node *:
       Equivalent to: dump_symtab_node (MSG_*, node)
   %E: gimple *:
       Equivalent to: dump_gimple_expr (MSG_*, TDF_SLIM, stmt, 0)
   %G: gimple *:
       Equivalent to: dump_gimple_stmt (MSG_*, TDF_SLIM, stmt, 0)
   %T: tree:
       Equivalent to: dump_generic_expr (MSG_*, arg, TDF_SLIM).

   TODO: add a format code that can handle (symtab_node*) *and* both
   subclasses (presumably means teaching -Wformat about non-virtual
   subclasses).

   These format codes build optinfo_item instances, thus capturing metadata
   about the arguments being dumped, as well as the textual output.  */

bool
dump_pretty_printer::decode_format (text_info *text, const char *spec,
				    pp_token_list &formatted_tok_list)
{
  /* Various format codes that imply making an optinfo_item and stashed it
     for later use (to capture metadata, rather than plain text).  */
  switch (*spec)
    {
    case 'C':
      {
	cgraph_node *node = va_arg (*text->m_args_ptr, cgraph_node *);

	/* Make an item for the node, and stash it.  */
	auto item = make_item_for_dump_symtab_node (node);
	stash_item (formatted_tok_list, std::move (item));
	return true;
      }

    case 'E':
      {
	gimple *stmt = va_arg (*text->m_args_ptr, gimple *);

	/* Make an item for the stmt, and stash it.  */
	auto item = make_item_for_dump_gimple_expr (stmt, 0, TDF_SLIM);
	stash_item (formatted_tok_list, std::move (item));
	return true;
      }

    case 'G':
      {
	gimple *stmt = va_arg (*text->m_args_ptr, gimple *);

	/* Make an item for the stmt, and stash it.  */
	auto item = make_item_for_dump_gimple_stmt (stmt, 0, TDF_SLIM);
	stash_item (formatted_tok_list, std::move (item));
	return true;
      }

    case 'T':
      {
	tree t = va_arg (*text->m_args_ptr, tree);

	/* Make an item for the tree, and stash it.  */
	auto item = make_item_for_dump_generic_expr (t, TDF_SLIM);
	stash_item (formatted_tok_list, std::move (item));
	return true;
      }

    default:
      return false;
    }
}

void
dump_pretty_printer::custom_token_printer::
print_tokens (pretty_printer *pp,
	      const pp_token_list &tokens)
{
  /* Accumulate text whilst emitting items.  */
  for (auto iter = tokens.m_first; iter; iter = iter->m_next)
    switch (iter->m_kind)
      {
      default:
	gcc_unreachable ();

      case pp_token::kind::text:
	{
	  pp_token_text *sub = as_a <pp_token_text *> (iter);
	  gcc_assert (sub->m_value.get ());
	  pp_string (pp, sub->m_value.get ());
	}
	break;

      case pp_token::kind::begin_color:
      case pp_token::kind::end_color:
	/* No-op for dumpfiles.  */
	break;

      case pp_token::kind::begin_quote:
	pp_begin_quote (pp, pp_show_color (pp));
	break;
      case pp_token::kind::end_quote:
	pp_end_quote (pp, pp_show_color (pp));
	break;

      case pp_token::kind::begin_url:
      case pp_token::kind::end_url:
	/* No-op for dumpfiles.  */
	break;

      case pp_token::kind::custom_data:
	{
	  emit_any_pending_textual_chunks ();
	  pp_token_custom_data *sub = as_a <pp_token_custom_data *> (iter);
	  gcc_assert (sub->m_value.get ());
	  wrapped_optinfo_item *custom_data
	    = static_cast<wrapped_optinfo_item *> (sub->m_value.get ());
	  m_dump_pp.emit_item (std::move (custom_data->m_optinfo_item),
			       m_optinfo);
	}
	break;
      }

  emit_any_pending_textual_chunks ();
}

/* Subroutine of dump_pretty_printer::custom_token_printer::print_tokens
   for consolidating multiple adjacent pure-text chunks into single
   optinfo_items (in phase 3).  */

void
dump_pretty_printer::custom_token_printer::
emit_any_pending_textual_chunks ()
{
  dump_pretty_printer *pp = &m_dump_pp;
  output_buffer *const buffer = pp_buffer (pp);
  gcc_assert (buffer->m_obstack == &buffer->m_formatted_obstack);

  /* Don't emit an item if the pending text is empty.  */
  if (output_buffer_last_position_in_text (buffer) == nullptr)
    return;

  char *formatted_text = xstrdup (pp_formatted_text (pp));
  std::unique_ptr<optinfo_item> item
    = std::make_unique<optinfo_item> (OPTINFO_ITEM_KIND_TEXT, UNKNOWN_LOCATION,
				      formatted_text);
  pp->emit_item (std::move (item), m_optinfo);

  /* Clear the pending text by unwinding formatted_text back to the start
     of the buffer (without deallocating).  */
  obstack_free (&buffer->m_formatted_obstack,
		buffer->m_formatted_obstack.object_base);
}

/* Output a formatted message using FORMAT on appropriate dump streams.  */

void
dump_context::dump_printf_va (const dump_metadata_t &metadata,
			      const char *format,
			      va_list *ap)
{
  dump_pretty_printer pp (this, metadata.get_dump_flags ());

  text_info text (format, ap, errno);

  /* Phases 1 and 2, using pp_format.  */
  pp_format (&pp, &text);

  /* Phase 3: update the custom token_printer with any active optinfo.  */
  if (optinfo_enabled_p ())
    {
      optinfo &info = ensure_pending_optinfo (metadata);
      pp.set_optinfo (&info);
    }
  else
    pp.set_optinfo (nullptr);

  pp_output_formatted_text (&pp, nullptr);
}

/* Similar to dump_printf, except source location is also printed, and
   dump location captured.  */

void
dump_context::dump_printf_loc_va (const dump_metadata_t &metadata,
				  const dump_user_location_t &loc,
				  const char *format, va_list *ap)
{
  dump_loc (metadata, loc);
  dump_printf_va (metadata, format, ap);
}

/* Make an item for the given dump call, equivalent to print_dec.  */

template<unsigned int N, typename C>
static std::unique_ptr<optinfo_item>
make_item_for_dump_dec (const poly_int<N, C> &value)
{
  STATIC_ASSERT (poly_coeff_traits<C>::signedness >= 0);
  signop sgn = poly_coeff_traits<C>::signedness ? SIGNED : UNSIGNED;

  pretty_printer pp;

  if (value.is_constant ())
    pp_wide_int (&pp, value.coeffs[0], sgn);
  else
    {
      pp_character (&pp, '[');
      for (unsigned int i = 0; i < N; ++i)
	{
	  pp_wide_int (&pp, value.coeffs[i], sgn);
	  pp_character (&pp, i == N - 1 ? ']' : ',');
	}
    }

  auto item
    = std::make_unique<optinfo_item> (OPTINFO_ITEM_KIND_TEXT, UNKNOWN_LOCATION,
				      xstrdup (pp_formatted_text (&pp)));
  return item;
}

/* Output VALUE in decimal to appropriate dump streams.  */

template<unsigned int N, typename C>
void
dump_context::dump_dec (const dump_metadata_t &metadata,
			const poly_int<N, C> &value)
{
  auto item = make_item_for_dump_dec (value);
  emit_item (*item.get (), metadata.get_dump_flags ());

  if (optinfo_enabled_p ())
    {
      optinfo &info = ensure_pending_optinfo (metadata);
      info.add_item (std::move (item));
    }
}

/* Output the name of NODE on appropriate dump streams.  */

void
dump_context::dump_symtab_node (const dump_metadata_t &metadata,
				symtab_node *node)
{
  auto item = make_item_for_dump_symtab_node (node);
  emit_item (*item.get (), metadata.get_dump_flags ());

  if (optinfo_enabled_p ())
    {
      optinfo &info = ensure_pending_optinfo (metadata);
      info.add_item (std::move (item));
    }
}

/* Get the current dump scope-nesting depth.
   For use by -fopt-info (for showing nesting via indentation).  */

unsigned int
dump_context::get_scope_depth () const
{
  return m_scope_depth;
}

/* Push a nested dump scope.
   Increment the scope depth.
   Print "=== NAME ===\n" to the dumpfile, if any, and to the -fopt-info
   destination, if any.
   Emit a "scope" optinfo if optinfos are enabled.  */

void
dump_context::begin_scope (const char *name,
			   const dump_user_location_t &user_location,
			   const dump_impl_location_t &impl_location)
{
  m_scope_depth++;

  location_t src_loc = user_location.get_location_t ();

  if (dump_file && apply_dump_filter_p (MSG_NOTE, pflags))
    ::dump_loc (MSG_NOTE, dump_file, src_loc);

  if (alt_dump_file && apply_dump_filter_p (MSG_NOTE, alt_flags))
    ::dump_loc (MSG_NOTE, alt_dump_file, src_loc);

  /* Support for temp_dump_context in selftests.  */
  if (m_test_pp && apply_dump_filter_p (MSG_NOTE, m_test_pp_flags))
    ::dump_loc (MSG_NOTE, m_test_pp, src_loc);

  /* Format multiple consecutive punctuation characters via %s to
     avoid -Wformat-diag in the pp_printf call below whose output
     isn't used for diagnostic output.  */
  pretty_printer pp;
  pp_printf (&pp, "%s %s %s", "===", name, "===");
  pp_newline (&pp);
  std::unique_ptr<optinfo_item> item
    = std::make_unique<optinfo_item> (OPTINFO_ITEM_KIND_TEXT, UNKNOWN_LOCATION,
				      xstrdup (pp_formatted_text (&pp)));
  emit_item (*item.get (), MSG_NOTE);

  if (optinfo_enabled_p ())
    {
      optinfo &info
	= begin_next_optinfo (dump_metadata_t (MSG_NOTE, impl_location),
			      user_location);
      info.m_kind = OPTINFO_KIND_SCOPE;
      info.add_item (std::move (item));
      end_any_optinfo ();
    }
}

/* Pop a nested dump scope.  */

void
dump_context::end_scope ()
{
  end_any_optinfo ();
  m_scope_depth--;

  if (m_json_writer)
    m_json_writer->pop_scope ();
}

/* Should optinfo instances be created?
   All creation of optinfos should be guarded by this predicate.
   Return true if any optinfo destinations are active.  */

bool
dump_context::optinfo_enabled_p () const
{
  return (optimization_records_enabled_p ());
}

/* Return the optinfo currently being accumulated, creating one if
   necessary.  */

optinfo &
dump_context::ensure_pending_optinfo (const dump_metadata_t &metadata)
{
  if (!m_pending)
    return begin_next_optinfo (metadata, dump_user_location_t ());
  return *m_pending;
}

/* Start a new optinfo and return it, ending any optinfo that was already
   accumulated.  */

optinfo &
dump_context::begin_next_optinfo (const dump_metadata_t &metadata,
				  const dump_user_location_t &user_loc)
{
  end_any_optinfo ();
  gcc_assert (m_pending == NULL);
  dump_location_t loc (user_loc, metadata.get_impl_location ());
  m_pending = new optinfo (loc, OPTINFO_KIND_NOTE, current_pass);
  m_pending->handle_dump_file_kind (metadata.get_dump_flags ());
  return *m_pending;
}

/* End any optinfo that has been accumulated within this context; emitting
   it to any destinations as appropriate, such as optimization records.  */

void
dump_context::end_any_optinfo ()
{
  if (m_pending)
    emit_optinfo (m_pending);
  delete m_pending;
  m_pending = NULL;
}

/* Emit the optinfo to all of the "non-immediate" destinations
   (emission to "immediate" destinations is done by
   dump_context::emit_item).  */

void
dump_context::emit_optinfo (const optinfo *info)
{
  /* -fsave-optimization-record.  */
  if (m_json_writer)
    m_json_writer->add_record (info);
}

/* Emit ITEM to all item destinations (those that don't require
   consolidation into optinfo instances).  */

void
dump_context::emit_item (const optinfo_item &item, dump_flags_t dump_kind)
{
  if (dump_file && apply_dump_filter_p (dump_kind, pflags))
    fprintf (dump_file, "%s", item.get_text ());

  if (alt_dump_file && apply_dump_filter_p (dump_kind, alt_flags))
    fprintf (alt_dump_file, "%s", item.get_text ());

  /* Support for temp_dump_context in selftests.  */
  if (m_test_pp && apply_dump_filter_p (dump_kind, m_test_pp_flags))
    pp_string (m_test_pp, item.get_text ());
}

/* The current singleton dump_context, and its default.  */

dump_context *dump_context::s_current = &dump_context::s_default;
dump_context dump_context::s_default;

/* Implementation of dump_* API calls, calling into dump_context
   member functions.  */

/* Calls to the dump_* functions do non-trivial work, so they ought
   to be guarded by:
     if (dump_enabled_p ())
   Assert that they are guarded, and, if assertions are disabled,
   bail out if the calls weren't properly guarded.  */

#define VERIFY_DUMP_ENABLED_P \
  do {					\
    gcc_assert (dump_enabled_p ());	\
    if (!dump_enabled_p ())		\
      return;				\
  } while (0)

/* Dump gimple statement GS with SPC indentation spaces and
   EXTRA_DUMP_FLAGS on the dump streams if DUMP_KIND is enabled.  */

void
dump_gimple_stmt (const dump_metadata_t &metadata, dump_flags_t extra_dump_flags,
		  gimple *gs, int spc)
{
  VERIFY_DUMP_ENABLED_P;
  dump_context::get ().dump_gimple_stmt (metadata, extra_dump_flags, gs, spc);
}

/* Similar to dump_gimple_stmt, except additionally print source location.  */

void
dump_gimple_stmt_loc (const dump_metadata_t &metadata,
		      const dump_user_location_t &loc,
		      dump_flags_t extra_dump_flags, gimple *gs, int spc)
{
  VERIFY_DUMP_ENABLED_P;
  dump_context::get ().dump_gimple_stmt_loc (metadata, loc, extra_dump_flags,
					     gs, spc);
}

/* Dump gimple statement GS with SPC indentation spaces and
   EXTRA_DUMP_FLAGS on the dump streams if DUMP_KIND is enabled.
   Do not terminate with a newline or semicolon.  */

void
dump_gimple_expr (const dump_metadata_t &metadata,
		  dump_flags_t extra_dump_flags,
		  gimple *gs, int spc)
{
  VERIFY_DUMP_ENABLED_P;
  dump_context::get ().dump_gimple_expr (metadata, extra_dump_flags, gs, spc);
}

/* Similar to dump_gimple_expr, except additionally print source location.  */

void
dump_gimple_expr_loc (const dump_metadata_t &metadata,
		      const dump_user_location_t &loc,
		      dump_flags_t extra_dump_flags, gimple *gs, int spc)
{
  VERIFY_DUMP_ENABLED_P;
  dump_context::get ().dump_gimple_expr_loc (metadata, loc, extra_dump_flags,
					     gs, spc);
}

/* Dump expression tree T using EXTRA_DUMP_FLAGS on dump streams if
   DUMP_KIND is enabled.  */

void
dump_generic_expr (const dump_metadata_t &metadata, dump_flags_t extra_dump_flags,
		   tree t)
{
  VERIFY_DUMP_ENABLED_P;
  dump_context::get ().dump_generic_expr (metadata, extra_dump_flags, t);
}

/* Similar to dump_generic_expr, except additionally print the source
   location.  */

void
dump_generic_expr_loc (const dump_metadata_t &metadata,
		       const dump_user_location_t &loc,
		       dump_flags_t extra_dump_flags, tree t)
{
  VERIFY_DUMP_ENABLED_P;
  dump_context::get ().dump_generic_expr_loc (metadata, loc, extra_dump_flags,
					      t);
}

/* Output a formatted message using FORMAT on appropriate dump streams.  */

void
dump_printf (const dump_metadata_t &metadata, const char *format, ...)
{
  VERIFY_DUMP_ENABLED_P;
  va_list ap;
  va_start (ap, format);
  dump_context::get ().dump_printf_va (metadata, format, &ap);
  va_end (ap);
}

/* Similar to dump_printf, except source location is also printed, and
   dump location captured.  */

void
dump_printf_loc (const dump_metadata_t &metadata,
		 const dump_user_location_t &loc,
		 const char *format, ...)
{
  VERIFY_DUMP_ENABLED_P;
  va_list ap;
  va_start (ap, format);
  dump_context::get ().dump_printf_loc_va (metadata, loc, format, &ap);
  va_end (ap);
}

/* Output VALUE in decimal to appropriate dump streams.  */

template<unsigned int N, typename C>
void
dump_dec (const dump_metadata_t &metadata, const poly_int<N, C> &value)
{
  VERIFY_DUMP_ENABLED_P;
  dump_context::get ().dump_dec (metadata, value);
}

template void dump_dec (const dump_metadata_t &metadata, const poly_uint16 &);
template void dump_dec (const dump_metadata_t &metadata, const poly_int64 &);
template void dump_dec (const dump_metadata_t &metadata, const poly_uint64 &);
template void dump_dec (const dump_metadata_t &metadata, const poly_offset_int &);
template void dump_dec (const dump_metadata_t &metadata, const poly_widest_int &);

void
dump_dec (dump_flags_t dump_kind, const poly_wide_int &value, signop sgn)
{
  VERIFY_DUMP_ENABLED_P;
  if (dump_file
      && dump_context::get ().apply_dump_filter_p (dump_kind, pflags))
    print_dec (value, dump_file, sgn);

  if (alt_dump_file
      && dump_context::get ().apply_dump_filter_p (dump_kind, alt_flags))
    print_dec (value, alt_dump_file, sgn);
}

/* Output VALUE in hexadecimal to appropriate dump streams.  */

void
dump_hex (dump_flags_t dump_kind, const poly_wide_int &value)
{
  VERIFY_DUMP_ENABLED_P;
  if (dump_file
      && dump_context::get ().apply_dump_filter_p (dump_kind, pflags))
    print_hex (value, dump_file);

  if (alt_dump_file
      && dump_context::get ().apply_dump_filter_p (dump_kind, alt_flags))
    print_hex (value, alt_dump_file);
}

/* Emit and delete the currently pending optinfo, if there is one,
   without the caller needing to know about class dump_context.  */

void
dumpfile_ensure_any_optinfo_are_flushed ()
{
  dump_context::get().end_any_optinfo ();
}

/* Output the name of NODE on appropriate dump streams.  */

void
dump_symtab_node (const dump_metadata_t &metadata, symtab_node *node)
{
  VERIFY_DUMP_ENABLED_P;
  dump_context::get ().dump_symtab_node (metadata, node);
}

/* Get the current dump scope-nesting depth.
   For use by -fopt-info (for showing nesting via indentation).  */

unsigned int
get_dump_scope_depth ()
{
  return dump_context::get ().get_scope_depth ();
}

/* Push a nested dump scope.
   Print "=== NAME ===\n" to the dumpfile, if any, and to the -fopt-info
   destination, if any.
   Emit a "scope" opinfo if optinfos are enabled.
   Increment the scope depth.  */

void
dump_begin_scope (const char *name,
		  const dump_user_location_t &user_location,
		  const dump_impl_location_t &impl_location)
{
  dump_context::get ().begin_scope (name, user_location, impl_location);
}

/* Pop a nested dump scope.  */

void
dump_end_scope ()
{
  dump_context::get ().end_scope ();
}

/* Start a dump for PHASE. Store user-supplied dump flags in
   *FLAG_PTR.  Return the number of streams opened.  Set globals
   DUMP_FILE, and ALT_DUMP_FILE to point to the opened streams, and
   set dump_flags appropriately for both pass dump stream and
   -fopt-info stream. */

int
gcc::dump_manager::
dump_start (int phase, dump_flags_t *flag_ptr)
{
  int count = 0;
  char *name;
  struct dump_file_info *dfi;
  FILE *stream;
  if (phase == TDI_none || !dump_phase_enabled_p (phase))
    return 0;

  dfi = get_dump_file_info (phase);
  name = get_dump_file_name (phase);
  if (name)
    {
      stream = dump_open (name, dfi->pstate < 0);
      if (stream)
        {
          dfi->pstate = 1;
          count++;
        }
      free (name);
      dfi->pstream = stream;
      set_dump_file (dfi->pstream);
      /* Initialize current dump flags. */
      pflags = dfi->pflags;
    }

  stream = dump_open_alternate_stream (dfi);
  if (stream)
    {
      dfi->alt_stream = stream;
      count++;
      set_alt_dump_file (dfi->alt_stream);
      /* Initialize current -fopt-info flags. */
      alt_flags = dfi->alt_flags;
    }

  if (flag_ptr)
    *flag_ptr = dfi->pflags;

  return count;
}

/* Finish a tree dump for PHASE and close associated dump streams.  Also
   reset the globals DUMP_FILE, ALT_DUMP_FILE, and DUMP_FLAGS.  */

void
gcc::dump_manager::
dump_finish (int phase)
{
  struct dump_file_info *dfi;

  if (phase < 0)
    return;
  dfi = get_dump_file_info (phase);
  if (dfi->pstream && dfi->pstream != stdout && dfi->pstream != stderr)
    fclose (dfi->pstream);

  if (dfi->alt_stream && dfi->alt_stream != stdout && dfi->alt_stream != stderr)
    fclose (dfi->alt_stream);

  dfi->alt_stream = NULL;
  dfi->pstream = NULL;
  set_dump_file (NULL);
  set_alt_dump_file (NULL);
  dump_flags = TDF_NONE;
  alt_flags = TDF_NONE;
  pflags = TDF_NONE;
}

/* Begin a tree dump for PHASE. Stores any user supplied flag in
   *FLAG_PTR and returns a stream to write to. If the dump is not
   enabled, returns NULL.
   PART can be used for dump files which should be split to multiple
   parts. PART == -1 indicates dump file with no parts.
   If PART is -1, multiple calls will reopen and append to the dump file.  */

FILE *
dump_begin (int phase, dump_flags_t *flag_ptr, int part)
{
  return g->get_dumps ()->dump_begin (phase, flag_ptr, part);
}

FILE *
gcc::dump_manager::
dump_begin (int phase, dump_flags_t *flag_ptr, int part)
{
  if (phase == TDI_none || !dump_phase_enabled_p (phase))
    return NULL;

  char *name = get_dump_file_name (phase, part);
  if (!name)
    return NULL;
  struct dump_file_info *dfi = get_dump_file_info (phase);

  /* We do not support re-opening of dump files with parts.  This would require
     tracking pstate per part of the dump file.  */
  FILE *stream = dump_open (name, part != -1 || dfi->pstate < 0);
  if (stream)
    dfi->pstate = 1;
  free (name);

  if (flag_ptr)
    *flag_ptr = dfi->pflags;

  /* Initialize current flags */
  pflags = dfi->pflags;
  return stream;
}

/* Returns nonzero if dump PHASE is enabled for at least one stream.
   If PHASE is TDI_tree_all, return nonzero if any dump is enabled for
   any phase.  */

int
gcc::dump_manager::
dump_phase_enabled_p (int phase) const
{
  if (phase == TDI_tree_all)
    {
      size_t i;
      for (i = TDI_none + 1; i < (size_t) TDI_end; i++)
	if (dump_files[i].pstate || dump_files[i].alt_state)
	  return 1;
      for (i = 0; i < m_extra_dump_files_in_use; i++)
	if (m_extra_dump_files[i].pstate || m_extra_dump_files[i].alt_state)
	  return 1;
      return 0;
    }
  else
    {
      struct dump_file_info *dfi = get_dump_file_info (phase);
      return dfi->pstate || dfi->alt_state;
    }
}

/* Returns nonzero if tree dump PHASE has been initialized.  */

int
gcc::dump_manager::
dump_initialized_p (int phase) const
{
  struct dump_file_info *dfi = get_dump_file_info (phase);
  return dfi->pstate > 0 || dfi->alt_state > 0;
}

/* Returns the switch name of PHASE.  */

const char *
dump_flag_name (int phase)
{
  return g->get_dumps ()->dump_flag_name (phase);
}

const char *
gcc::dump_manager::
dump_flag_name (int phase) const
{
  struct dump_file_info *dfi = get_dump_file_info (phase);
  return dfi->swtch;
}

/* Handle -fdump-* and -fopt-info for a pass added after
   command-line options are parsed (those from plugins and
   those from backends).

   Because the registration of plugin/backend passes happens after the
   command-line options are parsed, the options that specify single
   pass dumping (e.g. -fdump-tree-PASSNAME) cannot be used for new
   passes. Therefore we currently can only enable dumping of
   new passes when the 'dump-all' flags (e.g. -fdump-tree-all)
   are specified.  This is done here.

   Similarly, the saved -fopt-info options are wired up to the new pass.  */

void
gcc::dump_manager::register_pass (opt_pass *pass)
{
  gcc_assert (pass);

  register_one_dump_file (pass);

  dump_file_info *pass_dfi = get_dump_file_info (pass->static_pass_number);
  gcc_assert (pass_dfi);

  enum tree_dump_index tdi;
  if (pass->type == SIMPLE_IPA_PASS
      || pass->type == IPA_PASS)
    tdi = TDI_ipa_all;
  else if (pass->type == GIMPLE_PASS)
    tdi = TDI_tree_all;
  else
    tdi = TDI_rtl_all;
  const dump_file_info *tdi_dfi = get_dump_file_info (tdi);
  gcc_assert (tdi_dfi);

  /* Check if dump-all flag is specified.  */
  if (tdi_dfi->pstate)
    {
      pass_dfi->pstate = tdi_dfi->pstate;
      pass_dfi->pflags = tdi_dfi->pflags;
    }

  update_dfi_for_opt_info (pass_dfi);
}

/* Finish a tree dump for PHASE. STREAM is the stream created by
   dump_begin.  */

void
dump_end (int phase ATTRIBUTE_UNUSED, FILE *stream)
{
  if (stream != stderr && stream != stdout)
    fclose (stream);
}

/* Enable all tree dumps with FLAGS on FILENAME.  Return number of
   enabled tree dumps.  */

int
gcc::dump_manager::
dump_enable_all (dump_kind dkind, dump_flags_t flags, const char *filename)
{
  int n = 0;
  size_t i;

  for (i = TDI_none + 1; i < (size_t) TDI_end; i++)
    {
      if (dump_files[i].dkind == dkind)
        {
          const char *old_filename = dump_files[i].pfilename;
          dump_files[i].pstate = -1;
          dump_files[i].pflags |= flags;
          n++;
          /* Override the existing filename.  */
          if (filename)
            {
              dump_files[i].pfilename = xstrdup (filename);
              /* Since it is a command-line provided file, which is
                 common to all the phases, use it in append mode.  */
              dump_files[i].pstate = 1;
            }
          if (old_filename && filename != old_filename)
            free (CONST_CAST (char *, old_filename));
        }
    }

  for (i = 0; i < m_extra_dump_files_in_use; i++)
    {
      if (m_extra_dump_files[i].dkind == dkind)
        {
          const char *old_filename = m_extra_dump_files[i].pfilename;
          m_extra_dump_files[i].pstate = -1;
          m_extra_dump_files[i].pflags |= flags;
          n++;
          /* Override the existing filename.  */
          if (filename)
            {
              m_extra_dump_files[i].pfilename = xstrdup (filename);
              /* Since it is a command-line provided file, which is
                 common to all the phases, use it in append mode.  */
              m_extra_dump_files[i].pstate = 1;
            }
          if (old_filename && filename != old_filename)
            free (CONST_CAST (char *, old_filename));
        }
    }

  return n;
}

/* Enable -fopt-info dumps on all dump files matching OPTGROUP_FLAGS.
   Enable dumps with FLAGS on FILENAME.  Return the number of enabled
   dumps.  */

int
gcc::dump_manager::
opt_info_enable_passes (optgroup_flags_t optgroup_flags, dump_flags_t flags,
			const char *filename)
{
  int n = 0;

  m_optgroup_flags = optgroup_flags;
  m_optinfo_flags = flags;
  m_optinfo_filename = xstrdup (filename);

  for (size_t i = TDI_none + 1; i < (size_t) TDI_end; i++)
    if (update_dfi_for_opt_info (&dump_files[i]))
      n++;

  for (size_t i = 0; i < m_extra_dump_files_in_use; i++)
    if (update_dfi_for_opt_info (&m_extra_dump_files[i]))
      n++;

  return n;
}

/* Use the saved -fopt-info options to update DFI.
   Return true if the dump is enabled.  */

bool
gcc::dump_manager::update_dfi_for_opt_info (dump_file_info *dfi) const
{
  gcc_assert (dfi);

  if (!(dfi->optgroup_flags & m_optgroup_flags))
    return false;

  const char *old_filename = dfi->alt_filename;
  /* Since this file is shared among different passes, it
     should be opened in append mode.  */
  dfi->alt_state = 1;
  dfi->alt_flags |= m_optinfo_flags;
  /* Override the existing filename.  */
  if (m_optinfo_filename)
    dfi->alt_filename = xstrdup (m_optinfo_filename);
  if (old_filename && m_optinfo_filename != old_filename)
    free (CONST_CAST (char *, old_filename));

  return true;
}

/* Helper routine to parse -<dump format>[=filename]
   and return the corresponding dump flag.  If POS_P is non-NULL,
   assign start of filename into *POS_P.  */

dump_flags_t
parse_dump_option (const char *option_value, const char **pos_p)
{
  const char *ptr;
  dump_flags_t flags;

  ptr = option_value;
  if (pos_p)
    *pos_p = NULL;

  /* Retain "user-facing" and "internals" messages, but filter out
     those from an opt_problem being re-emitted at the top level
     (MSG_PRIORITY_REEMITTED), so as to avoid duplicate messages
     messing up scan-tree-dump-times" in DejaGnu tests.  */
  flags = MSG_PRIORITY_USER_FACING | MSG_PRIORITY_INTERNALS;

  while (*ptr)
    {
      const struct kv_pair<dump_flags_t> *option_ptr;
      const char *end_ptr;
      const char *eq_ptr;
      unsigned length;
      while (*ptr == '-')
	ptr++;
      end_ptr = strchr (ptr, '-');
      eq_ptr = strchr (ptr, '=');

      if (eq_ptr && (!end_ptr || end_ptr > eq_ptr))
	end_ptr = eq_ptr;

      if (!end_ptr)
	end_ptr = ptr + strlen (ptr);
      length = end_ptr - ptr;

      for (option_ptr = dump_options; option_ptr->name; option_ptr++)
	if (strlen (option_ptr->name) == length
	    && !memcmp (option_ptr->name, ptr, length))
	  {
	    flags |= option_ptr->value;
	    goto found;
	  }

      if (*ptr == '=')
	{
          /* Interpret rest of the argument as a dump filename.  This
             filename overrides other command line filenames.  */
	  if (pos_p)
	    *pos_p = ptr + 1;
	  break;
	}
      else
      {
	warning (0, "ignoring unknown option %q.*s",
		 length, ptr);
	flags = TDF_ERROR;
      }
    found:
      ptr = end_ptr;
  }

  return flags;
}

/* Parse ARG as a dump switch.  Return nonzero if it is, and store the
   relevant details in the dump_files array.  */

int
gcc::dump_manager::
dump_switch_p_1 (const char *arg, struct dump_file_info *dfi, bool doglob)
{
  const char *option_value;
  dump_flags_t flags = TDF_NONE;

  if (doglob && !dfi->glob)
    return 0;

  option_value = skip_leading_substring (arg, doglob ? dfi->glob : dfi->swtch);
  if (!option_value)
    return 0;

  if (*option_value && *option_value != '-' && *option_value != '=')
    return 0;

  const char *filename;
  flags = parse_dump_option (option_value, &filename);
  if (filename)
    {
      if (dfi->pfilename)
  free (CONST_CAST (char *, dfi->pfilename));
      dfi->pfilename = xstrdup (filename);
    }

  dfi->pstate = -1;
  dfi->pflags |= flags;

  /* Process -fdump-tree-all and -fdump-rtl-all, by enabling all the
     known dumps.  */
  if (dfi->suffix == NULL)
    dump_enable_all (dfi->dkind, dfi->pflags, dfi->pfilename);

  return 1;
}

void
gcc::dump_manager::
dump_switch_p (const char *arg)
{
  size_t i;
  int any = 0;

  for (i = TDI_none + 1; i != TDI_end; i++)
    any |= dump_switch_p_1 (arg, &dump_files[i], false);

  /* Don't glob if we got a hit already */
  if (!any)
    for (i = TDI_none + 1; i != TDI_end; i++)
      any |= dump_switch_p_1 (arg, &dump_files[i], true);

  for (i = 0; i < m_extra_dump_files_in_use; i++)
    any |= dump_switch_p_1 (arg, &m_extra_dump_files[i], false);

  if (!any)
    for (i = 0; i < m_extra_dump_files_in_use; i++)
      any |= dump_switch_p_1 (arg, &m_extra_dump_files[i], true);

  if (!any)
    {
      auto_vec<const char *> candidates;
      for (size_t i = TDI_none + 1; i != TDI_end; i++)
	candidates.safe_push (dump_files[i].swtch);
      for (size_t i = 0; i < m_extra_dump_files_in_use; i++)
	candidates.safe_push (m_extra_dump_files[i].swtch);
      const char *hint = find_closest_string (arg, &candidates);
      if (hint)
	error ("unrecognized command-line option %<-fdump-%s%>; "
	       "did you mean %<-fdump-%s%>?", arg, hint);
      else
	error ("unrecognized command-line option %<-fdump-%s%>", arg);
    }
}

/* Parse ARG as a -fopt-info switch and store flags, optgroup_flags
   and filename.  Return non-zero if it is a recognized switch.  */

static int
opt_info_switch_p_1 (const char *arg, dump_flags_t *flags,
		     optgroup_flags_t *optgroup_flags, char **filename)
{
  const char *option_value;
  const char *ptr;

  option_value = arg;
  ptr = option_value;

  *filename = NULL;

  /* Default to filtering out "internals" messages, and retaining
     "user-facing" messages, and those from an opt_problem being
     re-emitted at the top level.  */
  *flags = MSG_PRIORITY_USER_FACING | MSG_PRIORITY_REEMITTED;

  *optgroup_flags = OPTGROUP_NONE;

  if (!ptr)
    return 1;       /* Handle '-fopt-info' without any additional options.  */

  while (*ptr)
    {
      const char *end_ptr;
      const char *eq_ptr;
      unsigned length;

      while (*ptr == '-')
	ptr++;
      end_ptr = strchr (ptr, '-');
      eq_ptr = strchr (ptr, '=');

      if (eq_ptr && (!end_ptr || eq_ptr < end_ptr))
        end_ptr = eq_ptr;
      else if (!end_ptr)
	end_ptr = ptr + strlen (ptr);
      length = end_ptr - ptr;

      for (const kv_pair<dump_flags_t> *option_ptr = optinfo_verbosity_options;
	   option_ptr->name; option_ptr++)
	if (strlen (option_ptr->name) == length
	    && !memcmp (option_ptr->name, ptr, length))
          {
            *flags |= option_ptr->value;
	    goto found;
          }

      for (const kv_pair<optgroup_flags_t> *option_ptr = optgroup_options;
	   option_ptr->name; option_ptr++)
	if (strlen (option_ptr->name) == length
	    && !memcmp (option_ptr->name, ptr, length))
          {
            *optgroup_flags |= option_ptr->value;
	    goto found;
          }

      if (*ptr == '=')
        {
          /* Interpret rest of the argument as a dump filename.  This
             filename overrides other command line filenames.  */
          *filename = xstrdup (ptr + 1);
          break;
        }
      else
        {
          warning (0, "unknown option %q.*s in %<-fopt-info-%s%>",
                   length, ptr, arg);
          return 0;
        }
    found:;
      ptr = end_ptr;
    }

  return 1;
}

/* Return non-zero if ARG is a recognized switch for
   -fopt-info. Return zero otherwise.  */

int
opt_info_switch_p (const char *arg)
{
  dump_flags_t flags;
  optgroup_flags_t optgroup_flags;
  char *filename;
  static char *file_seen = NULL;
  gcc::dump_manager *dumps = g->get_dumps ();

  if (!opt_info_switch_p_1 (arg, &flags, &optgroup_flags, &filename))
    return 0;

  if (!filename)
    filename = xstrdup ("stderr");

  /* Bail out if a different filename has been specified.  */
  if (file_seen && strcmp (file_seen, filename))
    {
      warning (0, "ignoring possibly conflicting option %<-fopt-info-%s%>",
               arg);
      return 1;
    }

  file_seen = xstrdup (filename);
  if (!(flags & MSG_ALL_KINDS))
    flags |= MSG_OPTIMIZED_LOCATIONS;
  if (!optgroup_flags)
    optgroup_flags = OPTGROUP_ALL;

  return dumps->opt_info_enable_passes (optgroup_flags, flags, filename);
}

/* Print basic block on the dump streams.  */

void
dump_basic_block (dump_flags_t dump_kind, basic_block bb, int indent)
{
  if (dump_file
      && dump_context::get ().apply_dump_filter_p (dump_kind, pflags))
    dump_bb (dump_file, bb, indent, TDF_DETAILS);
  if (alt_dump_file
      && dump_context::get ().apply_dump_filter_p (dump_kind, alt_flags))
    dump_bb (alt_dump_file, bb, indent, TDF_DETAILS);
}

/* Dump FUNCTION_DECL FN as tree dump PHASE.  */

void
dump_function (int phase, tree fn)
{
  FILE *stream;
  dump_flags_t flags;

  stream = dump_begin (phase, &flags);
  if (stream)
    {
      dump_function_to_file (fn, stream, flags);
      dump_end (phase, stream);
    }
}

/* Enable RTL dump for all the RTL passes.  */

bool
enable_rtl_dump_file (void)
{
  gcc::dump_manager *dumps = g->get_dumps ();
  int num_enabled =
    dumps->dump_enable_all (DK_rtl, dump_flags_t (TDF_DETAILS) | TDF_BLOCKS,
			    NULL);
  return num_enabled > 0;
}

/* debug_dump_context's ctor.  Temporarily override the dump_context
   (to forcibly enable output to stderr).  */

debug_dump_context::debug_dump_context (FILE *f)
: m_context (),
  m_saved (&dump_context::get ()),
  m_saved_flags (dump_flags),
  m_saved_pflags (pflags),
  m_saved_file (dump_file)
{
  set_dump_file (f);
  dump_context::s_current = &m_context;
  pflags = dump_flags = MSG_ALL_KINDS | MSG_ALL_PRIORITIES;
  dump_context::get ().refresh_dumps_are_enabled ();
}

/* debug_dump_context's dtor.  Restore the saved dump_context.  */

debug_dump_context::~debug_dump_context ()
{
  set_dump_file (m_saved_file);
  dump_context::s_current = m_saved;
  dump_flags = m_saved_flags;
  pflags = m_saved_pflags;
  dump_context::get ().refresh_dumps_are_enabled ();
}


#if CHECKING_P

namespace selftest {

/* temp_dump_context's ctor.  Temporarily override the dump_context
   (to forcibly enable optinfo-generation).  */

temp_dump_context::temp_dump_context (bool forcibly_enable_optinfo,
				      bool forcibly_enable_dumping,
				      dump_flags_t test_pp_flags)
: m_context (),
  m_saved (&dump_context::get ())
{
  dump_context::s_current = &m_context;
  if (forcibly_enable_optinfo)
    m_context.set_json_writer (new optrecord_json_writer ());
  /* Conditionally enable the test dump, so that we can verify both the
     dump_enabled_p and the !dump_enabled_p cases in selftests.  */
  if (forcibly_enable_dumping)
    {
      m_context.m_test_pp = &m_pp;
      m_context.m_test_pp_flags = test_pp_flags;
    }

  dump_context::get ().refresh_dumps_are_enabled ();
}

/* temp_dump_context's dtor.  Restore the saved dump_context.  */

temp_dump_context::~temp_dump_context ()
{
  m_context.set_json_writer (NULL);

  dump_context::s_current = m_saved;

  dump_context::get ().refresh_dumps_are_enabled ();
}

/* 0-terminate the text dumped so far, and return it.  */

const char *
temp_dump_context::get_dumped_text ()
{
  return pp_formatted_text (&m_pp);
}

/* Verify that IMPL_LOC is within EXPECTED_FILE at EXPECTED_LINE,
   from EXPECTED_FUNCTION, using LOC for the location of any failure,
   provided that the build compiler is sufficiently recent.  */

static void
assert_impl_location_eq (const location &loc ATTRIBUTE_UNUSED,
			 const dump_impl_location_t &impl_loc ATTRIBUTE_UNUSED,
			 const char *expected_file ATTRIBUTE_UNUSED,
			 int expected_line ATTRIBUTE_UNUSED,
			 const char *expected_function ATTRIBUTE_UNUSED)
{
#if __GNUC__ > 4 || (__GNUC__ == 4 && __GNUC_MINOR__ >= 8)
  ASSERT_STR_CONTAINS_AT (loc, impl_loc.m_file, expected_file);
  ASSERT_EQ_AT (loc, impl_loc.m_line, expected_line);
  ASSERT_STR_CONTAINS_AT (loc, impl_loc.m_function, expected_function);
#endif
}

/* Verify that IMPL_LOC is within EXPECTED_FILE at EXPECTED_LINE,
   from EXPECTED_FUNCTION, provided that the build compiler is
   sufficiently recent.  */

#define ASSERT_IMPL_LOCATION_EQ(IMPL_LOC, EXPECTED_FILE, EXPECTED_LINE, \
				EXPECTED_FUNCTION)			\
  SELFTEST_BEGIN_STMT							\
    assert_impl_location_eq (SELFTEST_LOCATION, IMPL_LOC,		\
			     EXPECTED_FILE, EXPECTED_LINE,		\
			     EXPECTED_FUNCTION);			\
  SELFTEST_END_STMT

/* Verify that the dump_location_t constructors capture the source location
   at which they were called (provided that the build compiler is sufficiently
   recent).  */

static void
test_impl_location ()
{
  /* Default ctor.  */
  {
    dump_location_t loc;
    const int expected_line = __LINE__ - 1;
    ASSERT_IMPL_LOCATION_EQ (loc.get_impl_location (),
			     "dumpfile.cc", expected_line, "test_impl_location");
  }

  /* Constructing from a gimple.  */
  {
    dump_location_t loc ((gimple *)NULL);
    const int expected_line = __LINE__ - 1;
    ASSERT_IMPL_LOCATION_EQ (loc.get_impl_location (),
			     "dumpfile.cc", expected_line, "test_impl_location");
  }

  /* Constructing from an rtx_insn.  */
  {
    dump_location_t loc ((rtx_insn *)NULL);
    const int expected_line = __LINE__ - 1;
    ASSERT_IMPL_LOCATION_EQ (loc.get_impl_location (),
			     "dumpfile.cc", expected_line, "test_impl_location");
  }
}

/* Verify that the text dumped so far in CONTEXT equals
   EXPECTED_TEXT, using LOC for the location of any failure.
   As a side-effect, the internal buffer is 0-terminated.  */

void
verify_dumped_text (const location &loc,
		    temp_dump_context *context,
		    const char *expected_text)
{
  gcc_assert (context);
  ASSERT_STREQ_AT (loc, context->get_dumped_text (),
		   expected_text);
}

/* Verify that ITEM has the expected values.  */

void
verify_item (const location &loc,
	     const optinfo_item *item,
	     enum optinfo_item_kind expected_kind,
	     location_t expected_location,
	     const char *expected_text)
{
  ASSERT_EQ_AT (loc, item->get_kind (), expected_kind);
  ASSERT_EQ_AT (loc, item->get_location (), expected_location);
  ASSERT_STREQ_AT (loc, item->get_text (), expected_text);
}

/* Verify that calls to the dump_* API are captured and consolidated into
   optimization records. */

static void
test_capture_of_dump_calls (const line_table_case &case_)
{
  /* Generate a location_t for testing.  */
  line_table_test ltt (case_);
  linemap_add (line_table, LC_ENTER, false, "test.txt", 0);
  linemap_line_start (line_table, 5, 100);
  linemap_add (line_table, LC_LEAVE, false, NULL, 0);
  location_t decl_loc = linemap_position_for_column (line_table, 8);
  location_t stmt_loc = linemap_position_for_column (line_table, 10);
  if (stmt_loc > LINE_MAP_MAX_LOCATION_WITH_COLS)
    return;

  dump_user_location_t loc = dump_user_location_t::from_location_t (stmt_loc);

  gimple *stmt = gimple_build_return (NULL);
  gimple_set_location (stmt, stmt_loc);

  tree test_decl = build_decl (decl_loc, FUNCTION_DECL,
			       get_identifier ("test_decl"),
			       build_function_type_list (void_type_node,
							 NULL_TREE));

  symbol_table_test tmp_symtab;

  cgraph_node *node = cgraph_node::get_create (test_decl);
  gcc_assert (node);

  /* Run all tests twice, with and then without optinfo enabled, to ensure
     that immediate destinations vs optinfo-based destinations both
     work, independently of each other, with no leaks.  */
  for (int i = 0 ; i < 2; i++)
    {
      bool with_optinfo = (i == 0);

      /* Test of dump_printf.  */
      {
	temp_dump_context tmp (with_optinfo, true,
			       MSG_ALL_KINDS | MSG_PRIORITY_USER_FACING);
	dump_printf (MSG_NOTE, "int: %i str: %s", 42, "foo");
	const int expected_impl_line = __LINE__ - 1;

	ASSERT_DUMPED_TEXT_EQ (tmp, "int: 42 str: foo");
	if (with_optinfo)
	  {
	    optinfo *info = tmp.get_pending_optinfo ();
	    ASSERT_TRUE (info != NULL);
	    ASSERT_EQ (info->get_kind (), OPTINFO_KIND_NOTE);
	    ASSERT_EQ (info->num_items (), 1);
	    ASSERT_IS_TEXT (info->get_item (0), "int: 42 str: foo");
	    ASSERT_IMPL_LOCATION_EQ (info->get_impl_location (),
				     "dumpfile.cc", expected_impl_line,
				     "test_capture_of_dump_calls");
	  }
      }

      /* Test of dump_printf with %T.  */
      {
	temp_dump_context tmp (with_optinfo, true,
			       MSG_ALL_KINDS | MSG_PRIORITY_USER_FACING);
	dump_printf (MSG_NOTE, "tree: %T", integer_zero_node);
	const int expected_impl_line = __LINE__ - 1;

	ASSERT_DUMPED_TEXT_EQ (tmp, "tree: 0");
	if (with_optinfo)
	  {
	    optinfo *info = tmp.get_pending_optinfo ();
	    ASSERT_TRUE (info != NULL);
	    ASSERT_EQ (info->get_kind (), OPTINFO_KIND_NOTE);
	    ASSERT_EQ (info->num_items (), 2);
	    ASSERT_IS_TEXT (info->get_item (0), "tree: ");
	    ASSERT_IS_TREE (info->get_item (1), UNKNOWN_LOCATION, "0");
	    ASSERT_IMPL_LOCATION_EQ (info->get_impl_location (),
				     "dumpfile.cc", expected_impl_line,
				     "test_capture_of_dump_calls");
	  }
      }

      /* Test of dump_printf with %E.  */
      {
	temp_dump_context tmp (with_optinfo, true,
			       MSG_ALL_KINDS | MSG_PRIORITY_USER_FACING);
	dump_printf (MSG_NOTE, "gimple: %E", stmt);
	const int expected_impl_line = __LINE__ - 1;

	ASSERT_DUMPED_TEXT_EQ (tmp, "gimple: return;");
	if (with_optinfo)
	  {
	    optinfo *info = tmp.get_pending_optinfo ();
	    ASSERT_TRUE (info != NULL);
	    ASSERT_EQ (info->get_kind (), OPTINFO_KIND_NOTE);
	    ASSERT_EQ (info->num_items (), 2);
	    ASSERT_IS_TEXT (info->get_item (0), "gimple: ");
	    ASSERT_IS_GIMPLE (info->get_item (1), stmt_loc, "return;");
	    ASSERT_IMPL_LOCATION_EQ (info->get_impl_location (),
				     "dumpfile.cc", expected_impl_line,
				     "test_capture_of_dump_calls");
	  }
      }

      /* Test of dump_printf with %G.  */
      {
	temp_dump_context tmp (with_optinfo, true,
			       MSG_ALL_KINDS | MSG_PRIORITY_USER_FACING);
	dump_printf (MSG_NOTE, "gimple: %G", stmt);
	const int expected_impl_line = __LINE__ - 1;

	ASSERT_DUMPED_TEXT_EQ (tmp, "gimple: return;\n");
	if (with_optinfo)
	  {
	    optinfo *info = tmp.get_pending_optinfo ();
	    ASSERT_TRUE (info != NULL);
	    ASSERT_EQ (info->get_kind (), OPTINFO_KIND_NOTE);
	    ASSERT_EQ (info->num_items (), 2);
	    ASSERT_IS_TEXT (info->get_item (0), "gimple: ");
	    ASSERT_IS_GIMPLE (info->get_item (1), stmt_loc, "return;\n");
	    ASSERT_IMPL_LOCATION_EQ (info->get_impl_location (),
				     "dumpfile.cc", expected_impl_line,
				     "test_capture_of_dump_calls");
	  }
      }

      /* Test of dump_printf with %C.  */
      {
	temp_dump_context tmp (with_optinfo, true,
			       MSG_ALL_KINDS | MSG_PRIORITY_USER_FACING);
	dump_printf (MSG_NOTE, "node: %C", node);
	const int expected_impl_line = __LINE__ - 1;

	ASSERT_DUMPED_TEXT_EQ (tmp, "node: test_decl/1");
	if (with_optinfo)
	  {
	    optinfo *info = tmp.get_pending_optinfo ();
	    ASSERT_TRUE (info != NULL);
	    ASSERT_EQ (info->get_kind (), OPTINFO_KIND_NOTE);
	    ASSERT_EQ (info->num_items (), 2);
	    ASSERT_IS_TEXT (info->get_item (0), "node: ");
	    ASSERT_IS_SYMTAB_NODE (info->get_item (1), decl_loc, "test_decl/1");
	    ASSERT_IMPL_LOCATION_EQ (info->get_impl_location (),
				     "dumpfile.cc", expected_impl_line,
				     "test_capture_of_dump_calls");
	  }
      }

      /* dump_print_loc with multiple format codes.  This tests various
	 things:
	 - intermingling of text, format codes handled by the base
	 pretty_printer, and dump-specific format codes
	 - multiple dump-specific format codes: some consecutive, others
	 separated by text, trailing text after the final one.  */
      {
	temp_dump_context tmp (with_optinfo, true,
			       MSG_ALL_KINDS | MSG_PRIORITY_USER_FACING);
	dump_printf_loc (MSG_NOTE, loc, "before %T and %T"
			 " %i consecutive %E%E after\n",
			 integer_zero_node, test_decl, 42, stmt, stmt);

	ASSERT_DUMPED_TEXT_EQ (tmp,
			       "test.txt:5:10: note: before 0 and test_decl"
			       " 42 consecutive return;return; after\n");
	if (with_optinfo)
	  {
	    optinfo *info = tmp.get_pending_optinfo ();
	    ASSERT_TRUE (info != NULL);
	    ASSERT_EQ (info->get_kind (), OPTINFO_KIND_NOTE);
	    ASSERT_EQ (info->num_items (), 8);
	    ASSERT_IS_TEXT (info->get_item (0), "before ");
	    ASSERT_IS_TREE (info->get_item (1), UNKNOWN_LOCATION, "0");
	    ASSERT_IS_TEXT (info->get_item (2), " and ");
	    ASSERT_IS_TREE (info->get_item (3), UNKNOWN_LOCATION, "test_decl");
	    ASSERT_IS_TEXT (info->get_item (4), " 42 consecutive ");
	    ASSERT_IS_GIMPLE (info->get_item (5), stmt_loc, "return;");
	    ASSERT_IS_GIMPLE (info->get_item (6), stmt_loc, "return;");
	    ASSERT_IS_TEXT (info->get_item (7), " after\n");
	    /* We don't ASSERT_IMPL_LOCATION_EQ here, to avoid having to
	       enforce at which exact line the multiline dump_printf_loc
	       occurred.  */
	  }
      }

      /* Tree, via dump_generic_expr.  */
      {
	temp_dump_context tmp (with_optinfo, true,
			       MSG_ALL_KINDS | MSG_PRIORITY_USER_FACING);
	dump_printf_loc (MSG_NOTE, loc, "test of tree: ");
	const int expected_impl_line = __LINE__ - 1;
	dump_generic_expr (MSG_NOTE, TDF_SLIM, integer_zero_node);

	ASSERT_DUMPED_TEXT_EQ (tmp, "test.txt:5:10: note: test of tree: 0");
	if (with_optinfo)
	  {
	    optinfo *info = tmp.get_pending_optinfo ();
	    ASSERT_TRUE (info != NULL);
	    ASSERT_EQ (info->get_location_t (), stmt_loc);
	    ASSERT_EQ (info->get_kind (), OPTINFO_KIND_NOTE);
	    ASSERT_EQ (info->num_items (), 2);
	    ASSERT_IS_TEXT (info->get_item (0), "test of tree: ");
	    ASSERT_IS_TREE (info->get_item (1), UNKNOWN_LOCATION, "0");
	    ASSERT_IMPL_LOCATION_EQ (info->get_impl_location (),
				     "dumpfile.cc", expected_impl_line,
				     "test_capture_of_dump_calls");
	  }
      }

      /* Tree, via dump_generic_expr_loc.  */
      {
	temp_dump_context tmp (with_optinfo, true,
			       MSG_ALL_KINDS | MSG_PRIORITY_USER_FACING);
	dump_generic_expr_loc (MSG_NOTE, loc, TDF_SLIM, integer_one_node);
	const int expected_impl_line = __LINE__ - 1;

	ASSERT_DUMPED_TEXT_EQ (tmp, "test.txt:5:10: note: 1");
	if (with_optinfo)
	  {
	    optinfo *info = tmp.get_pending_optinfo ();
	    ASSERT_TRUE (info != NULL);
	    ASSERT_EQ (info->get_location_t (), stmt_loc);
	    ASSERT_EQ (info->get_kind (), OPTINFO_KIND_NOTE);
	    ASSERT_EQ (info->num_items (), 1);
	    ASSERT_IS_TREE (info->get_item (0), UNKNOWN_LOCATION, "1");
	    ASSERT_IMPL_LOCATION_EQ (info->get_impl_location (),
				     "dumpfile.cc", expected_impl_line,
				     "test_capture_of_dump_calls");
	  }
      }

      /* Gimple.  */
      {
	/* dump_gimple_stmt_loc.  */
	{
	  temp_dump_context tmp (with_optinfo, true,
				 MSG_ALL_KINDS | MSG_PRIORITY_USER_FACING);
	  dump_gimple_stmt_loc (MSG_NOTE, loc, TDF_SLIM, stmt, 2);
	  const int expected_impl_line = __LINE__ - 1;

	  ASSERT_DUMPED_TEXT_EQ (tmp, "test.txt:5:10: note: return;\n");
	  if (with_optinfo)
	    {
	      optinfo *info = tmp.get_pending_optinfo ();
	      ASSERT_TRUE (info != NULL);
	      ASSERT_EQ (info->num_items (), 1);
	      ASSERT_IS_GIMPLE (info->get_item (0), stmt_loc, "return;\n");
	      ASSERT_IMPL_LOCATION_EQ (info->get_impl_location (),
				       "dumpfile.cc", expected_impl_line,
				       "test_capture_of_dump_calls");
	    }
	}

	/* dump_gimple_stmt.  */
	{
	  temp_dump_context tmp (with_optinfo, true,
				 MSG_ALL_KINDS | MSG_PRIORITY_USER_FACING);
	  dump_gimple_stmt (MSG_NOTE, TDF_SLIM, stmt, 2);
	  const int expected_impl_line = __LINE__ - 1;

	  ASSERT_DUMPED_TEXT_EQ (tmp, "return;\n");
	  if (with_optinfo)
	    {
	      optinfo *info = tmp.get_pending_optinfo ();
	      ASSERT_TRUE (info != NULL);
	      ASSERT_EQ (info->num_items (), 1);
	      ASSERT_IS_GIMPLE (info->get_item (0), stmt_loc, "return;\n");
	      ASSERT_IMPL_LOCATION_EQ (info->get_impl_location (),
				       "dumpfile.cc", expected_impl_line,
				       "test_capture_of_dump_calls");
	    }
	}

	/* dump_gimple_expr_loc.  */
	{
	  temp_dump_context tmp (with_optinfo, true,
				 MSG_ALL_KINDS | MSG_PRIORITY_USER_FACING);
	  dump_gimple_expr_loc (MSG_NOTE, loc, TDF_SLIM, stmt, 2);
	  const int expected_impl_line = __LINE__ - 1;

	  ASSERT_DUMPED_TEXT_EQ (tmp, "test.txt:5:10: note: return;");
	  if (with_optinfo)
	    {
	      optinfo *info = tmp.get_pending_optinfo ();
	      ASSERT_TRUE (info != NULL);
	      ASSERT_EQ (info->num_items (), 1);
	      ASSERT_IS_GIMPLE (info->get_item (0), stmt_loc, "return;");
	      ASSERT_IMPL_LOCATION_EQ (info->get_impl_location (),
				       "dumpfile.cc", expected_impl_line,
				       "test_capture_of_dump_calls");
	    }
	}

	/* dump_gimple_expr.  */
	{
	  temp_dump_context tmp (with_optinfo, true,
				 MSG_ALL_KINDS | MSG_PRIORITY_USER_FACING);
	  dump_gimple_expr (MSG_NOTE, TDF_SLIM, stmt, 2);
	  const int expected_impl_line = __LINE__ - 1;

	  ASSERT_DUMPED_TEXT_EQ (tmp, "return;");
	  if (with_optinfo)
	    {
	      optinfo *info = tmp.get_pending_optinfo ();
	      ASSERT_TRUE (info != NULL);
	      ASSERT_EQ (info->num_items (), 1);
	      ASSERT_IS_GIMPLE (info->get_item (0), stmt_loc, "return;");
	      ASSERT_IMPL_LOCATION_EQ (info->get_impl_location (),
				       "dumpfile.cc", expected_impl_line,
				       "test_capture_of_dump_calls");
	    }
	}
      }

      /* symtab_node.  */
      {
	temp_dump_context tmp (with_optinfo, true,
			       MSG_ALL_KINDS | MSG_PRIORITY_USER_FACING);
	dump_symtab_node (MSG_NOTE, node);
	const int expected_impl_line = __LINE__ - 1;

	ASSERT_DUMPED_TEXT_EQ (tmp, "test_decl/1");
	if (with_optinfo)
	  {
	    optinfo *info = tmp.get_pending_optinfo ();
	    ASSERT_TRUE (info != NULL);
	    ASSERT_EQ (info->get_kind (), OPTINFO_KIND_NOTE);
	    ASSERT_EQ (info->num_items (), 1);
	    ASSERT_IS_SYMTAB_NODE (info->get_item (0), decl_loc, "test_decl/1");
	    ASSERT_IMPL_LOCATION_EQ (info->get_impl_location (),
				     "dumpfile.cc", expected_impl_line,
				     "test_capture_of_dump_calls");
	  }
      }

      /* poly_int.  */
      {
	temp_dump_context tmp (with_optinfo, true,
			       MSG_ALL_KINDS | MSG_PRIORITY_USER_FACING);
	dump_dec (MSG_NOTE, poly_int64 (42));
	const int expected_impl_line = __LINE__ - 1;

	ASSERT_DUMPED_TEXT_EQ (tmp, "42");
	if (with_optinfo)
	  {
	    optinfo *info = tmp.get_pending_optinfo ();
	    ASSERT_TRUE (info != NULL);
	    ASSERT_EQ (info->num_items (), 1);
	    ASSERT_IS_TEXT (info->get_item (0), "42");
	    ASSERT_IMPL_LOCATION_EQ (info->get_impl_location (),
				     "dumpfile.cc", expected_impl_line,
				     "test_capture_of_dump_calls");
	  }
      }

      /* Scopes.  Test with all 4 combinations of
	 filtering by MSG_PRIORITY_USER_FACING
	 and/or filtering by MSG_PRIORITY_INTERNALS.  */
      for (int j = 0; j < 3; j++)
	{
	  dump_flags_t dump_filter = MSG_ALL_KINDS;
	  if (j % 2)
	    dump_filter |= MSG_PRIORITY_USER_FACING;
	  if (j / 2)
	    dump_filter |= MSG_PRIORITY_INTERNALS;

	  temp_dump_context tmp (with_optinfo, true, dump_filter);
	  /* Emit various messages, mostly with implicit priority.  */
	  dump_printf_loc (MSG_NOTE, stmt, "msg 1\n");
	  dump_printf_loc (MSG_NOTE | MSG_PRIORITY_INTERNALS, stmt,
			   "explicitly internal msg\n");
	  {
	    AUTO_DUMP_SCOPE ("outer scope", stmt);
	    dump_printf_loc (MSG_NOTE, stmt, "msg 2\n");
	    {
	      AUTO_DUMP_SCOPE ("middle scope", stmt);
	      dump_printf_loc (MSG_NOTE, stmt, "msg 3\n");
	      {
		AUTO_DUMP_SCOPE ("inner scope", stmt);
		dump_printf_loc (MSG_NOTE, stmt, "msg 4\n");
		dump_printf_loc (MSG_NOTE | MSG_PRIORITY_USER_FACING, stmt,
				 "explicitly user-facing msg\n");
	      }
	      dump_printf_loc (MSG_NOTE, stmt, "msg 5\n");
	    }
	    dump_printf_loc (MSG_NOTE, stmt, "msg 6\n");
	  }
	  dump_printf_loc (MSG_NOTE, stmt, "msg 7\n");
	  const int expected_impl_line = __LINE__ - 1;

	  switch (dump_filter & MSG_ALL_PRIORITIES)
	    {
	    default:
	      gcc_unreachable ();
	    case 0:
	      ASSERT_DUMPED_TEXT_EQ (tmp, "");
	      break;
	    case MSG_PRIORITY_USER_FACING:
	      ASSERT_DUMPED_TEXT_EQ
		(tmp,
		 "test.txt:5:10: note: msg 1\n"
		 "test.txt:5:10: note:    explicitly user-facing msg\n"
		 "test.txt:5:10: note: msg 7\n");
	      break;
	    case MSG_PRIORITY_INTERNALS:
	      ASSERT_DUMPED_TEXT_EQ
		(tmp,
		 "test.txt:5:10: note: explicitly internal msg\n"
		 "test.txt:5:10: note:  === outer scope ===\n"
		 "test.txt:5:10: note:  msg 2\n"
		 "test.txt:5:10: note:   === middle scope ===\n"
		 "test.txt:5:10: note:   msg 3\n"
		 "test.txt:5:10: note:    === inner scope ===\n"
		 "test.txt:5:10: note:    msg 4\n"
		 "test.txt:5:10: note:   msg 5\n"
		 "test.txt:5:10: note:  msg 6\n");
	      break;
	    case MSG_ALL_PRIORITIES:
	      ASSERT_DUMPED_TEXT_EQ
		(tmp,
		 "test.txt:5:10: note: msg 1\n"
		 "test.txt:5:10: note: explicitly internal msg\n"
		 "test.txt:5:10: note: === outer scope ===\n"
		 "test.txt:5:10: note:  msg 2\n"
		 "test.txt:5:10: note:  === middle scope ===\n"
		 "test.txt:5:10: note:   msg 3\n"
		 "test.txt:5:10: note:   === inner scope ===\n"
		 "test.txt:5:10: note:    msg 4\n"
		 "test.txt:5:10: note:    explicitly user-facing msg\n"
		 "test.txt:5:10: note:   msg 5\n"
		 "test.txt:5:10: note:  msg 6\n"
		 "test.txt:5:10: note: msg 7\n");
	      break;
	    }
	  if (with_optinfo)
	    {
	      optinfo *info = tmp.get_pending_optinfo ();
	      ASSERT_TRUE (info != NULL);
	      ASSERT_EQ (info->num_items (), 1);
	      ASSERT_IS_TEXT (info->get_item (0), "msg 7\n");
	      ASSERT_IMPL_LOCATION_EQ (info->get_impl_location (),
				       "dumpfile.cc", expected_impl_line,
				       "test_capture_of_dump_calls");
	    }
	}
    }

  /* Verify that MSG_* affects optinfo->get_kind (); we tested MSG_NOTE
     above.  */
  {
    /* MSG_OPTIMIZED_LOCATIONS.  */
    {
      temp_dump_context tmp (true, true, MSG_ALL_KINDS);
      dump_printf_loc (MSG_OPTIMIZED_LOCATIONS, loc, "test");
      ASSERT_EQ (tmp.get_pending_optinfo ()->get_kind (),
		 OPTINFO_KIND_SUCCESS);
    }

    /* MSG_MISSED_OPTIMIZATION.  */
    {
      temp_dump_context tmp (true, true, MSG_ALL_KINDS);
      dump_printf_loc (MSG_MISSED_OPTIMIZATION, loc, "test");
      ASSERT_EQ (tmp.get_pending_optinfo ()->get_kind (),
		 OPTINFO_KIND_FAILURE);
    }
  }

  /* Verify that MSG_* affect AUTO_DUMP_SCOPE and the dump calls.  */
  {
    temp_dump_context tmp (false, true,
			   MSG_OPTIMIZED_LOCATIONS | MSG_ALL_PRIORITIES);
    dump_printf_loc (MSG_NOTE, stmt, "msg 1\n");
    {
      AUTO_DUMP_SCOPE ("outer scope", stmt);
      dump_printf_loc (MSG_NOTE, stmt, "msg 2\n");
      {
	AUTO_DUMP_SCOPE ("middle scope", stmt);
	dump_printf_loc (MSG_NOTE, stmt, "msg 3\n");
	{
	  AUTO_DUMP_SCOPE ("inner scope", stmt);
	  dump_printf_loc (MSG_OPTIMIZED_LOCATIONS, stmt, "msg 4\n");
	}
	dump_printf_loc (MSG_NOTE, stmt, "msg 5\n");
      }
      dump_printf_loc (MSG_NOTE, stmt, "msg 6\n");
    }
    dump_printf_loc (MSG_NOTE, stmt, "msg 7\n");

    ASSERT_DUMPED_TEXT_EQ (tmp, "test.txt:5:10: optimized:    msg 4\n");
  }
}

static void
test_pr87025 ()
{
  dump_user_location_t loc
    = dump_user_location_t::from_location_t (UNKNOWN_LOCATION);

  temp_dump_context tmp (true, true,
			 MSG_ALL_KINDS | MSG_PRIORITY_USER_FACING);
  {
    AUTO_DUMP_SCOPE ("outer scope", loc);
    dump_printf (MSG_NOTE, "msg1\n");
  }
}

/* Run all of the selftests within this file.  */

void
dumpfile_cc_tests ()
{
  test_impl_location ();
  for_each_line_table_case (test_capture_of_dump_calls);
  test_pr87025 ();
}

} // namespace selftest

#endif /* CHECKING_P */
