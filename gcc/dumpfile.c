/* Dump infrastructure for optimizations and intermediate representation.
   Copyright (C) 2012-2018 Free Software Foundation, Inc.

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

/* If non-NULL, return one past-the-end of the matching SUBPART of
   the WHOLE string.  */
#define skip_leading_substring(whole,  part) \
   (strncmp (whole, part, strlen (part)) ? NULL : whole + strlen (part))

static dump_flags_t pflags;		      /* current dump_flags */

static void dump_loc (dump_flags_t, FILE *, source_location);

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
#define FIRST_AUTO_NUMBERED_DUMP 1
#define FIRST_ME_AUTO_NUMBERED_DUMP 4

  DUMP_FILE_INFO (NULL, "lang-all", DK_lang, 0),
  DUMP_FILE_INFO (NULL, "tree-all", DK_tree, 0),
  DUMP_FILE_INFO (NULL, "rtl-all", DK_rtl, 0),
  DUMP_FILE_INFO (NULL, "ipa-all", DK_ipa, 0),
};

/* Table of dump options. This must be consistent with the TDF_* flags
   in dumpfile.h and opt_info_options below. */
static const kv_pair<dump_flags_t> dump_options[] =
{
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
  {"optall", MSG_ALL},
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
  {"all", MSG_ALL},
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
  m_extra_dump_files_alloced (0)
{
}

gcc::dump_manager::~dump_manager ()
{
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

/* Print source location on DFILE if enabled.  */

static void
dump_loc (dump_flags_t dump_kind, FILE *dfile, source_location loc)
{
  if (dump_kind)
    {
      if (LOCATION_LOCUS (loc) > BUILTINS_LOCATION)
        fprintf (dfile, "%s:%d:%d: note: ", LOCATION_FILE (loc),
                 LOCATION_LINE (loc), LOCATION_COLUMN (loc));
      else if (current_function_decl)
        fprintf (dfile, "%s:%d:%d: note: ",
                 DECL_SOURCE_FILE (current_function_decl),
                 DECL_SOURCE_LINE (current_function_decl),
                 DECL_SOURCE_COLUMN (current_function_decl));
      /* Indentation based on scope depth.  */
      fprintf (dfile, "%*s", get_dump_scope_depth (), "");
    }
}

/* Print source location to PP if enabled.  */

static void
dump_loc (dump_flags_t dump_kind, pretty_printer *pp, source_location loc)
{
  if (dump_kind)
    {
      if (LOCATION_LOCUS (loc) > BUILTINS_LOCATION)
	pp_printf (pp, "%s:%d:%d: note: ", LOCATION_FILE (loc),
		   LOCATION_LINE (loc), LOCATION_COLUMN (loc));
      else if (current_function_decl)
	pp_printf (pp, "%s:%d:%d: note: ",
		   DECL_SOURCE_FILE (current_function_decl),
		   DECL_SOURCE_LINE (current_function_decl),
		   DECL_SOURCE_COLUMN (current_function_decl));
      /* Indentation based on scope depth.  */
      for (unsigned i = 0; i < get_dump_scope_depth (); i++)
	pp_character (pp, ' ');
    }
}

/* Implementation of dump_context member functions.  */

/* dump_context's dtor.  */

dump_context::~dump_context ()
{
  delete m_pending;
}

/* Update the "dumps_are_enabled" global; to be called whenever dump_file
   or alt_dump_file change, or when changing dump_context in selftests.  */

void
dump_context::refresh_dumps_are_enabled ()
{
  dumps_are_enabled = (dump_file || alt_dump_file || optinfo_enabled_p ()
		       || m_test_pp);
}

/* Print LOC to the appropriate dump destinations, given DUMP_KIND.
   If optinfos are enabled, begin a new optinfo.  */

void
dump_context::dump_loc (dump_flags_t dump_kind, const dump_location_t &loc)
{
  end_any_optinfo ();

  location_t srcloc = loc.get_location_t ();

  if (dump_file && (dump_kind & pflags))
    ::dump_loc (dump_kind, dump_file, srcloc);

  if (alt_dump_file && (dump_kind & alt_flags))
    ::dump_loc (dump_kind, alt_dump_file, srcloc);

  /* Support for temp_dump_context in selftests.  */
  if (m_test_pp && (dump_kind & m_test_pp_flags))
    ::dump_loc (dump_kind, m_test_pp, srcloc);

  if (optinfo_enabled_p ())
    {
      optinfo &info = begin_next_optinfo (loc);
      info.handle_dump_file_kind (dump_kind);
    }
}

/* Make an item for the given dump call, equivalent to print_gimple_stmt.  */

static optinfo_item *
make_item_for_dump_gimple_stmt (gimple *stmt, int spc, dump_flags_t dump_flags)
{
  pretty_printer pp;
  pp_needs_newline (&pp) = true;
  pp_gimple_stmt_1 (&pp, stmt, spc, dump_flags);
  pp_newline (&pp);

  optinfo_item *item
    = new optinfo_item (OPTINFO_ITEM_KIND_GIMPLE, gimple_location (stmt),
			xstrdup (pp_formatted_text (&pp)));
  return item;
}

/* Dump gimple statement GS with SPC indentation spaces and
   EXTRA_DUMP_FLAGS on the dump streams if DUMP_KIND is enabled.  */

void
dump_context::dump_gimple_stmt (dump_flags_t dump_kind,
				dump_flags_t extra_dump_flags,
				gimple *gs, int spc)
{
  optinfo_item *item
    = make_item_for_dump_gimple_stmt (gs, spc, dump_flags | extra_dump_flags);
  emit_item (item, dump_kind);

  if (optinfo_enabled_p ())
    {
      optinfo &info = ensure_pending_optinfo ();
      info.handle_dump_file_kind (dump_kind);
      info.add_item (item);
    }
  else
    delete item;
}

/* Similar to dump_gimple_stmt, except additionally print source location.  */

void
dump_context::dump_gimple_stmt_loc (dump_flags_t dump_kind,
				    const dump_location_t &loc,
				    dump_flags_t extra_dump_flags,
				    gimple *gs, int spc)
{
  dump_loc (dump_kind, loc);
  dump_gimple_stmt (dump_kind, extra_dump_flags, gs, spc);
}

/* Make an item for the given dump call, equivalent to print_gimple_expr.  */

static optinfo_item *
make_item_for_dump_gimple_expr (gimple *stmt, int spc, dump_flags_t dump_flags)
{
  dump_flags |= TDF_RHS_ONLY;
  pretty_printer pp;
  pp_needs_newline (&pp) = true;
  pp_gimple_stmt_1 (&pp, stmt, spc, dump_flags);

  optinfo_item *item
    = new optinfo_item (OPTINFO_ITEM_KIND_GIMPLE, gimple_location (stmt),
			xstrdup (pp_formatted_text (&pp)));
  return item;
}

/* Dump gimple statement GS with SPC indentation spaces and
   EXTRA_DUMP_FLAGS on the dump streams if DUMP_KIND is enabled.
   Do not terminate with a newline or semicolon.  */

void
dump_context::dump_gimple_expr (dump_flags_t dump_kind,
				dump_flags_t extra_dump_flags,
				gimple *gs, int spc)
{
  optinfo_item *item
    = make_item_for_dump_gimple_expr (gs, spc, dump_flags | extra_dump_flags);
  emit_item (item, dump_kind);

  if (optinfo_enabled_p ())
    {
      optinfo &info = ensure_pending_optinfo ();
      info.handle_dump_file_kind (dump_kind);
      info.add_item (item);
    }
  else
    delete item;
}

/* Similar to dump_gimple_expr, except additionally print source location.  */

void
dump_context::dump_gimple_expr_loc (dump_flags_t dump_kind,
				    const dump_location_t &loc,
				    dump_flags_t extra_dump_flags,
				    gimple *gs,
				    int spc)
{
  dump_loc (dump_kind, loc);
  dump_gimple_expr (dump_kind, extra_dump_flags, gs, spc);
}

/* Make an item for the given dump call, equivalent to print_generic_expr.  */

static optinfo_item *
make_item_for_dump_generic_expr (tree node, dump_flags_t dump_flags)
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
			xstrdup (pp_formatted_text (&pp)));
  return item;
}

/* Dump expression tree T using EXTRA_DUMP_FLAGS on dump streams if
   DUMP_KIND is enabled.  */

void
dump_context::dump_generic_expr (dump_flags_t dump_kind,
				 dump_flags_t extra_dump_flags,
				 tree t)
{
  optinfo_item *item
    = make_item_for_dump_generic_expr (t, dump_flags | extra_dump_flags);
  emit_item (item, dump_kind);

  if (optinfo_enabled_p ())
    {
      optinfo &info = ensure_pending_optinfo ();
      info.handle_dump_file_kind (dump_kind);
      info.add_item (item);
    }
  else
    delete item;
}


/* Similar to dump_generic_expr, except additionally print the source
   location.  */

void
dump_context::dump_generic_expr_loc (dump_flags_t dump_kind,
				     const dump_location_t &loc,
				     dump_flags_t extra_dump_flags,
				     tree t)
{
  dump_loc (dump_kind, loc);
  dump_generic_expr (dump_kind, extra_dump_flags, t);
}

/* A subclass of pretty_printer for implementing dump_context::dump_printf_va.
   In particular, the formatted chunks are captured as optinfo_item instances,
   thus retaining metadata about the entities being dumped (e.g. source
   locations), rather than just as plain text.  */

class dump_pretty_printer : public pretty_printer
{
public:
  dump_pretty_printer (dump_context *context, dump_flags_t dump_kind);

  void emit_items (optinfo *dest);

private:
  /* Information on an optinfo_item that was generated during phase 2 of
     formatting.  */
  struct stashed_item
  {
    stashed_item (const char **buffer_ptr_, optinfo_item *item_)
      : buffer_ptr (buffer_ptr_), item (item_) {}
    const char **buffer_ptr;
    optinfo_item *item;
  };

  static bool format_decoder_cb (pretty_printer *pp, text_info *text,
				 const char *spec, int /*precision*/,
				 bool /*wide*/, bool /*set_locus*/,
				 bool /*verbose*/, bool */*quoted*/,
				 const char **buffer_ptr);

  bool decode_format (text_info *text, const char *spec,
		      const char **buffer_ptr);

  void stash_item (const char **buffer_ptr, optinfo_item *item);

  void emit_any_pending_textual_chunks (optinfo *dest);

  void emit_item (optinfo_item *item, optinfo *dest);

  dump_context *m_context;
  dump_flags_t m_dump_kind;
  auto_vec<stashed_item> m_stashed_items;
};

/* dump_pretty_printer's ctor.  */

dump_pretty_printer::dump_pretty_printer (dump_context *context,
					  dump_flags_t dump_kind)
: pretty_printer (), m_context (context), m_dump_kind (dump_kind),
  m_stashed_items ()
{
  pp_format_decoder (this) = format_decoder_cb;
}

/* Phase 3 of formatting; compare with pp_output_formatted_text.

   Emit optinfo_item instances for the various formatted chunks from phases
   1 and 2 (i.e. pp_format).

   Some chunks may already have had their items built (during decode_format).
   These chunks have been stashed into m_stashed_items; we emit them here.

   For all other purely textual chunks, they are printed into
   buffer->formatted_obstack, and then emitted as a textual optinfo_item.
   This consolidates multiple adjacent text chunks into a single text
   optinfo_item.  */

void
dump_pretty_printer::emit_items (optinfo *dest)
{
  output_buffer *buffer = pp_buffer (this);
  struct chunk_info *chunk_array = buffer->cur_chunk_array;
  const char **args = chunk_array->args;

  gcc_assert (buffer->obstack == &buffer->formatted_obstack);
  gcc_assert (buffer->line_length == 0);

  unsigned stashed_item_idx = 0;
  for (unsigned chunk = 0; args[chunk]; chunk++)
    {
      if (stashed_item_idx < m_stashed_items.length ()
	  && args[chunk] == *m_stashed_items[stashed_item_idx].buffer_ptr)
	{
	  emit_any_pending_textual_chunks (dest);
	  /* This chunk has a stashed item: use it.  */
	  emit_item (m_stashed_items[stashed_item_idx++].item, dest);
	}
      else
	/* This chunk is purely textual.  Print it (to
	   buffer->formatted_obstack), so that we can consolidate adjacent
	   chunks into one textual optinfo_item.  */
	pp_string (this, args[chunk]);
    }

  emit_any_pending_textual_chunks (dest);

  /* Ensure that we consumed all of stashed_items.  */
  gcc_assert (stashed_item_idx == m_stashed_items.length ());

  /* Deallocate the chunk structure and everything after it (i.e. the
     associated series of formatted strings).  */
  buffer->cur_chunk_array = chunk_array->prev;
  obstack_free (&buffer->chunk_obstack, chunk_array);
}

/* Subroutine of dump_pretty_printer::emit_items
   for consolidating multiple adjacent pure-text chunks into single
   optinfo_items (in phase 3).  */

void
dump_pretty_printer::emit_any_pending_textual_chunks (optinfo *dest)
{
  gcc_assert (buffer->obstack == &buffer->formatted_obstack);

  /* Don't emit an item if the pending text is empty.  */
  if (output_buffer_last_position_in_text (buffer) == NULL)
    return;

  char *formatted_text = xstrdup (pp_formatted_text (this));
  optinfo_item *item
    = new optinfo_item (OPTINFO_ITEM_KIND_TEXT, UNKNOWN_LOCATION,
			formatted_text);
  emit_item (item, dest);

  /* Clear the pending text by unwinding formatted_text back to the start
     of the buffer (without deallocating).  */
  obstack_free (&buffer->formatted_obstack,
		buffer->formatted_obstack.object_base);
}

/* Emit ITEM and take ownership of it.  If DEST is non-NULL, add ITEM
   to DEST; otherwise delete ITEM.  */

void
dump_pretty_printer::emit_item (optinfo_item *item, optinfo *dest)
{
  m_context->emit_item (item, m_dump_kind);
  if (dest)
    dest->add_item (item);
  else
    delete item;
}

/* Record that ITEM (generated in phase 2 of formatting) is to be used for
   the chunk at BUFFER_PTR in phase 3 (by emit_items).  */

void
dump_pretty_printer::stash_item (const char **buffer_ptr, optinfo_item *item)
{
  gcc_assert (buffer_ptr);
  gcc_assert (item);

  m_stashed_items.safe_push (stashed_item (buffer_ptr, item));
}

/* pp_format_decoder callback for dump_pretty_printer, and thus for
   dump_printf and dump_printf_loc.

   A wrapper around decode_format, for type-safety.  */

bool
dump_pretty_printer::format_decoder_cb (pretty_printer *pp, text_info *text,
					const char *spec, int /*precision*/,
					bool /*wide*/, bool /*set_locus*/,
					bool /*verbose*/, bool */*quoted*/,
					const char **buffer_ptr)
{
  dump_pretty_printer *opp = static_cast <dump_pretty_printer *> (pp);
  return opp->decode_format (text, spec, buffer_ptr);
}

/* Format decoder for dump_pretty_printer, and thus for dump_printf and
   dump_printf_loc.

   Supported format codes (in addition to the standard pretty_printer ones)
   are:

   %E: gimple *:
       Equivalent to: dump_gimple_expr (MSG_*, TDF_SLIM, stmt, 0)
   %G: gimple *:
       Equivalent to: dump_gimple_stmt (MSG_*, TDF_SLIM, stmt, 0)
   %T: tree:
       Equivalent to: dump_generic_expr (MSG_*, arg, TDF_SLIM).

   FIXME: add symtab_node?

   These format codes build optinfo_item instances, thus capturing metadata
   about the arguments being dumped, as well as the textual output.  */

bool
dump_pretty_printer::decode_format (text_info *text, const char *spec,
				       const char **buffer_ptr)
{
  /* Various format codes that imply making an optinfo_item and stashed it
     for later use (to capture metadata, rather than plain text).  */
  switch (*spec)
    {
    case 'E':
      {
	gimple *stmt = va_arg (*text->args_ptr, gimple *);

	/* Make an item for the stmt, and stash it.  */
	optinfo_item *item = make_item_for_dump_gimple_expr (stmt, 0, TDF_SLIM);
	stash_item (buffer_ptr, item);
	return true;
      }

    case 'G':
      {
	gimple *stmt = va_arg (*text->args_ptr, gimple *);

	/* Make an item for the stmt, and stash it.  */
	optinfo_item *item = make_item_for_dump_gimple_stmt (stmt, 0, TDF_SLIM);
	stash_item (buffer_ptr, item);
	return true;
      }

    case 'T':
      {
	tree t = va_arg (*text->args_ptr, tree);

	/* Make an item for the tree, and stash it.  */
	optinfo_item *item = make_item_for_dump_generic_expr (t, TDF_SLIM);
	stash_item (buffer_ptr, item);
	return true;
      }

    default:
      return false;
    }
}

/* Output a formatted message using FORMAT on appropriate dump streams.  */

void
dump_context::dump_printf_va (dump_flags_t dump_kind, const char *format,
			      va_list *ap)
{
  dump_pretty_printer pp (this, dump_kind);

  text_info text;
  text.err_no = errno;
  text.args_ptr = ap;
  text.format_spec = format;

  /* Phases 1 and 2, using pp_format.  */
  pp_format (&pp, &text);

  /* Phase 3.  */
  if (optinfo_enabled_p ())
    {
      optinfo &info = ensure_pending_optinfo ();
      info.handle_dump_file_kind (dump_kind);
      pp.emit_items (&info);
    }
  else
    pp.emit_items (NULL);
}

/* Similar to dump_printf, except source location is also printed, and
   dump location captured.  */

void
dump_context::dump_printf_loc_va (dump_flags_t dump_kind,
				  const dump_location_t &loc,
				  const char *format, va_list *ap)
{
  dump_loc (dump_kind, loc);
  dump_printf_va (dump_kind, format, ap);
}

/* Make an item for the given dump call, equivalent to print_dec.  */

template<unsigned int N, typename C>
static optinfo_item *
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

  optinfo_item *item
    = new optinfo_item (OPTINFO_ITEM_KIND_TEXT, UNKNOWN_LOCATION,
			xstrdup (pp_formatted_text (&pp)));
  return item;
}

/* Output VALUE in decimal to appropriate dump streams.  */

template<unsigned int N, typename C>
void
dump_context::dump_dec (dump_flags_t dump_kind, const poly_int<N, C> &value)
{
  optinfo_item *item = make_item_for_dump_dec (value);
  emit_item (item, dump_kind);

  if (optinfo_enabled_p ())
    {
      optinfo &info = ensure_pending_optinfo ();
      info.handle_dump_file_kind (dump_kind);
      info.add_item (item);
    }
  else
    delete item;
}

/* Make an item for the given dump call.  */

static optinfo_item *
make_item_for_dump_symtab_node (symtab_node *node)
{
  location_t loc = DECL_SOURCE_LOCATION (node->decl);
  optinfo_item *item
    = new optinfo_item (OPTINFO_ITEM_KIND_SYMTAB_NODE, loc,
			xstrdup (node->dump_name ()));
  return item;
}

/* Output the name of NODE on appropriate dump streams.  */

void
dump_context::dump_symtab_node (dump_flags_t dump_kind, symtab_node *node)
{
  optinfo_item *item = make_item_for_dump_symtab_node (node);
  emit_item (item, dump_kind);

  if (optinfo_enabled_p ())
    {
      optinfo &info = ensure_pending_optinfo ();
      info.handle_dump_file_kind (dump_kind);
      info.add_item (item);
    }
  else
    delete item;
}

/* Get the current dump scope-nesting depth.
   For use by -fopt-info (for showing nesting via indentation).  */

unsigned int
dump_context::get_scope_depth () const
{
  return m_scope_depth;
}

/* Push a nested dump scope.
   Print "=== NAME ===\n" to the dumpfile, if any, and to the -fopt-info
   destination, if any.
   Emit a "scope" optinfo if optinfos are enabled.
   Increment the scope depth.  */

void
dump_context::begin_scope (const char *name, const dump_location_t &loc)
{
  if (dump_file)
    ::dump_loc (MSG_NOTE, dump_file, loc.get_location_t ());

  if (alt_dump_file)
    ::dump_loc (MSG_NOTE, alt_dump_file, loc.get_location_t ());

  /* Support for temp_dump_context in selftests.  */
  if (m_test_pp)
    ::dump_loc (MSG_NOTE, m_test_pp, loc.get_location_t ());

  pretty_printer pp;
  pp_printf (&pp, "=== %s ===\n", name);
  optinfo_item *item
    = new optinfo_item (OPTINFO_ITEM_KIND_TEXT, UNKNOWN_LOCATION,
			xstrdup (pp_formatted_text (&pp)));
  emit_item (item, MSG_NOTE);

  if (optinfo_enabled_p ())
    {
      optinfo &info = begin_next_optinfo (loc);
      info.m_kind = OPTINFO_KIND_SCOPE;
      info.add_item (item);
    }
  else
    delete item;

  m_scope_depth++;
}

/* Pop a nested dump scope.  */

void
dump_context::end_scope ()
{
  end_any_optinfo ();
  m_scope_depth--;
  optimization_records_maybe_pop_dump_scope ();
}

/* Return the optinfo currently being accumulated, creating one if
   necessary.  */

optinfo &
dump_context::ensure_pending_optinfo ()
{
  if (!m_pending)
    return begin_next_optinfo (dump_location_t (dump_user_location_t ()));
  return *m_pending;
}

/* Start a new optinfo and return it, ending any optinfo that was already
   accumulated.  */

optinfo &
dump_context::begin_next_optinfo (const dump_location_t &loc)
{
  end_any_optinfo ();
  gcc_assert (m_pending == NULL);
  m_pending = new optinfo (loc, OPTINFO_KIND_NOTE, current_pass);
  return *m_pending;
}

/* End any optinfo that has been accumulated within this context; emitting
   it to any destinations as appropriate, such as optimization records.  */

void
dump_context::end_any_optinfo ()
{
  if (m_pending)
    m_pending->emit ();
  delete m_pending;
  m_pending = NULL;
}

/* Emit ITEM to all item destinations (those that don't require
   consolidation into optinfo instances).  */

void
dump_context::emit_item (optinfo_item *item, dump_flags_t dump_kind)
{
  if (dump_file && (dump_kind & pflags))
    fprintf (dump_file, "%s", item->get_text ());

  if (alt_dump_file && (dump_kind & alt_flags))
    fprintf (alt_dump_file, "%s", item->get_text ());

  /* Support for temp_dump_context in selftests.  */
  if (m_test_pp && (dump_kind & m_test_pp_flags))
    pp_string (m_test_pp, item->get_text ());
}

/* The current singleton dump_context, and its default.  */

dump_context *dump_context::s_current = &dump_context::s_default;
dump_context dump_context::s_default;

/* Implementation of dump_* API calls, calling into dump_context
   member functions.  */

/* Dump gimple statement GS with SPC indentation spaces and
   EXTRA_DUMP_FLAGS on the dump streams if DUMP_KIND is enabled.  */

void
dump_gimple_stmt (dump_flags_t dump_kind, dump_flags_t extra_dump_flags,
		  gimple *gs, int spc)
{
  dump_context::get ().dump_gimple_stmt (dump_kind, extra_dump_flags, gs, spc);
}

/* Similar to dump_gimple_stmt, except additionally print source location.  */

void
dump_gimple_stmt_loc (dump_flags_t dump_kind, const dump_location_t &loc,
		      dump_flags_t extra_dump_flags, gimple *gs, int spc)
{
  dump_context::get ().dump_gimple_stmt_loc (dump_kind, loc, extra_dump_flags,
					     gs, spc);
}

/* Dump gimple statement GS with SPC indentation spaces and
   EXTRA_DUMP_FLAGS on the dump streams if DUMP_KIND is enabled.
   Do not terminate with a newline or semicolon.  */

void
dump_gimple_expr (dump_flags_t dump_kind, dump_flags_t extra_dump_flags,
		  gimple *gs, int spc)
{
  dump_context::get ().dump_gimple_expr (dump_kind, extra_dump_flags, gs, spc);
}

/* Similar to dump_gimple_expr, except additionally print source location.  */

void
dump_gimple_expr_loc (dump_flags_t dump_kind, const dump_location_t &loc,
		      dump_flags_t extra_dump_flags, gimple *gs, int spc)
{
  dump_context::get ().dump_gimple_expr_loc (dump_kind, loc, extra_dump_flags,
					     gs, spc);
}

/* Dump expression tree T using EXTRA_DUMP_FLAGS on dump streams if
   DUMP_KIND is enabled.  */

void
dump_generic_expr (dump_flags_t dump_kind, dump_flags_t extra_dump_flags,
		   tree t)
{
  dump_context::get ().dump_generic_expr (dump_kind, extra_dump_flags, t);
}

/* Similar to dump_generic_expr, except additionally print the source
   location.  */

void
dump_generic_expr_loc (dump_flags_t dump_kind, const dump_location_t &loc,
		       dump_flags_t extra_dump_flags, tree t)
{
  dump_context::get ().dump_generic_expr_loc (dump_kind, loc, extra_dump_flags,
					      t);
}

/* Output a formatted message using FORMAT on appropriate dump streams.  */

void
dump_printf (dump_flags_t dump_kind, const char *format, ...)
{
  va_list ap;
  va_start (ap, format);
  dump_context::get ().dump_printf_va (dump_kind, format, &ap);
  va_end (ap);
}

/* Similar to dump_printf, except source location is also printed, and
   dump location captured.  */

void
dump_printf_loc (dump_flags_t dump_kind, const dump_location_t &loc,
		 const char *format, ...)
{
  va_list ap;
  va_start (ap, format);
  dump_context::get ().dump_printf_loc_va (dump_kind, loc, format, &ap);
  va_end (ap);
}

/* Output VALUE in decimal to appropriate dump streams.  */

template<unsigned int N, typename C>
void
dump_dec (dump_flags_t dump_kind, const poly_int<N, C> &value)
{
  dump_context::get ().dump_dec (dump_kind, value);
}

template void dump_dec (dump_flags_t, const poly_uint16 &);
template void dump_dec (dump_flags_t, const poly_int64 &);
template void dump_dec (dump_flags_t, const poly_uint64 &);
template void dump_dec (dump_flags_t, const poly_offset_int &);
template void dump_dec (dump_flags_t, const poly_widest_int &);

void
dump_dec (dump_flags_t dump_kind, const poly_wide_int &value, signop sgn)
{
  if (dump_file && (dump_kind & pflags))
    print_dec (value, dump_file, sgn);

  if (alt_dump_file && (dump_kind & alt_flags))
    print_dec (value, alt_dump_file, sgn);
}

/* Output VALUE in hexadecimal to appropriate dump streams.  */

void
dump_hex (dump_flags_t dump_kind, const poly_wide_int &value)
{
  if (dump_file && (dump_kind & pflags))
    print_hex (value, dump_file);

  if (alt_dump_file && (dump_kind & alt_flags))
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
dump_symtab_node (dump_flags_t dump_kind, symtab_node *node)
{
  dump_context::get ().dump_symtab_node (dump_kind, node);
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
dump_begin_scope (const char *name, const dump_location_t &loc)
{
  dump_context::get ().begin_scope (name, loc);
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
  char *name;
  struct dump_file_info *dfi;
  FILE *stream;

  if (phase == TDI_none || !dump_phase_enabled_p (phase))
    return NULL;

  name = get_dump_file_name (phase, part);
  if (!name)
    return NULL;
  dfi = get_dump_file_info (phase);

  /* We do not support re-opening of dump files with parts.  This would require
     tracking pstate per part of the dump file.  */
  stream = dump_open (name, part != -1 || dfi->pstate < 0);
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
      if ((dump_files[i].dkind == dkind))
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
      if ((m_extra_dump_files[i].dkind == dkind))
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
  size_t i;

  for (i = TDI_none + 1; i < (size_t) TDI_end; i++)
    {
      if ((dump_files[i].optgroup_flags & optgroup_flags))
        {
          const char *old_filename = dump_files[i].alt_filename;
          /* Since this file is shared among different passes, it
             should be opened in append mode.  */
          dump_files[i].alt_state = 1;
          dump_files[i].alt_flags |= flags;
          n++;
          /* Override the existing filename.  */
          if (filename)
            dump_files[i].alt_filename = xstrdup (filename);
          if (old_filename && filename != old_filename)
            free (CONST_CAST (char *, old_filename));
        }
    }

  for (i = 0; i < m_extra_dump_files_in_use; i++)
    {
      if ((m_extra_dump_files[i].optgroup_flags & optgroup_flags))
        {
          const char *old_filename = m_extra_dump_files[i].alt_filename;
          /* Since this file is shared among different passes, it
             should be opened in append mode.  */
          m_extra_dump_files[i].alt_state = 1;
          m_extra_dump_files[i].alt_flags |= flags;
          n++;
          /* Override the existing filename.  */
          if (filename)
            m_extra_dump_files[i].alt_filename = xstrdup (filename);
          if (old_filename && filename != old_filename)
            free (CONST_CAST (char *, old_filename));
        }
    }

  return n;
}

/* Parse ARG as a dump switch. Return nonzero if it is, and store the
   relevant details in the dump_files array.  */

int
gcc::dump_manager::
dump_switch_p_1 (const char *arg, struct dump_file_info *dfi, bool doglob)
{
  const char *option_value;
  const char *ptr;
  dump_flags_t flags;

  if (doglob && !dfi->glob)
    return 0;

  option_value = skip_leading_substring (arg, doglob ? dfi->glob : dfi->swtch);
  if (!option_value)
    return 0;

  if (*option_value && *option_value != '-' && *option_value != '=')
    return 0;

  ptr = option_value;
  flags = TDF_NONE;

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

      if (eq_ptr && !end_ptr)
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
          if (dfi->pfilename)
            free (CONST_CAST (char *, dfi->pfilename));
          dfi->pfilename = xstrdup (ptr + 1);
          break;
        }
      else
        warning (0, "ignoring unknown option %q.*s in %<-fdump-%s%>",
                 length, ptr, dfi->swtch);
    found:;
      ptr = end_ptr;
    }

  dfi->pstate = -1;
  dfi->pflags |= flags;

  /* Process -fdump-tree-all and -fdump-rtl-all, by enabling all the
     known dumps.  */
  if (dfi->suffix == NULL)
    dump_enable_all (dfi->dkind, dfi->pflags, dfi->pfilename);

  return 1;
}

int
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


  return any;
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
  *flags = TDF_NONE;
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

      if (eq_ptr && !end_ptr)
        end_ptr = eq_ptr;

      if (!end_ptr)
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
  if (!flags)
    flags = MSG_OPTIMIZED_LOCATIONS;
  if (!optgroup_flags)
    optgroup_flags = OPTGROUP_ALL;

  return dumps->opt_info_enable_passes (optgroup_flags, flags, filename);
}

/* Print basic block on the dump streams.  */

void
dump_basic_block (dump_flags_t dump_kind, basic_block bb, int indent)
{
  if (dump_file && (dump_kind & pflags))
    dump_bb (dump_file, bb, indent, TDF_DETAILS);
  if (alt_dump_file && (dump_kind & alt_flags))
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

/* Print information from the combine pass on dump_file.  */

void
print_combine_total_stats (void)
{
  if (dump_file)
    dump_combine_total_stats (dump_file);
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

#if CHECKING_P

/* temp_dump_context's ctor.  Temporarily override the dump_context
   (to forcibly enable optinfo-generation).  */

temp_dump_context::temp_dump_context (bool forcibly_enable_optinfo,
				      dump_flags_t test_pp_flags)

: m_context (),
  m_saved (&dump_context ().get ())
{
  dump_context::s_current = &m_context;
  m_context.m_forcibly_enable_optinfo = forcibly_enable_optinfo;
  m_context.m_test_pp = &m_pp;
  m_context.m_test_pp_flags = test_pp_flags;

  dump_context::get ().refresh_dumps_are_enabled ();
}

/* temp_dump_context's dtor.  Restore the saved dump_context.  */

temp_dump_context::~temp_dump_context ()
{
  dump_context::s_current = m_saved;

  dump_context::get ().refresh_dumps_are_enabled ();
}

/* 0-terminate the text dumped so far, and return it.  */

const char *
temp_dump_context::get_dumped_text ()
{
  return pp_formatted_text (&m_pp);
}

namespace selftest {

/* Verify that the dump_location_t constructors capture the source location
   at which they were called (provided that the build compiler is sufficiently
   recent).  */

static void
test_impl_location ()
{
#if __GNUC__ > 4 || (__GNUC__ == 4 && __GNUC_MINOR__ >= 8)
  /* Default ctor.  */
  {
    dump_location_t loc;
    const int expected_line = __LINE__ - 1;
    ASSERT_STR_CONTAINS (loc.get_impl_location ().m_file, "dumpfile.c");
    ASSERT_EQ (loc.get_impl_location ().m_line, expected_line);
  }

  /* Constructing from a gimple.  */
  {
    dump_location_t loc ((gimple *)NULL);
    const int expected_line = __LINE__ - 1;
    ASSERT_STR_CONTAINS (loc.get_impl_location ().m_file, "dumpfile.c");
    ASSERT_EQ (loc.get_impl_location ().m_line, expected_line);
  }

  /* Constructing from an rtx_insn.  */
  {
    dump_location_t loc ((rtx_insn *)NULL);
    const int expected_line = __LINE__ - 1;
    ASSERT_STR_CONTAINS (loc.get_impl_location ().m_file, "dumpfile.c");
    ASSERT_EQ (loc.get_impl_location ().m_line, expected_line);
  }
#endif
}

/* Verify that the text dumped so far in CONTEXT equals
   EXPECTED_TEXT, using LOC for the location of any failure.
   As a side-effect, the internal buffer is 0-terminated.  */

static void
verify_dumped_text (const location &loc,
		    temp_dump_context *context,
		    const char *expected_text)
{
  gcc_assert (context);
  ASSERT_STREQ_AT (loc, context->get_dumped_text (),
		   expected_text);
}

/* Verify that the text dumped so far in CONTEXT equals
   EXPECTED_TEXT.
   As a side-effect, the internal buffer is 0-terminated.  */

#define ASSERT_DUMPED_TEXT_EQ(CONTEXT, EXPECTED_TEXT)			\
  SELFTEST_BEGIN_STMT							\
    verify_dumped_text (SELFTEST_LOCATION, &(CONTEXT), (EXPECTED_TEXT)); \
  SELFTEST_END_STMT

/* Verify that ITEM has the expected values.  */

static void
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

/* Verify that ITEM is a text item, with EXPECTED_TEXT.  */

#define ASSERT_IS_TEXT(ITEM, EXPECTED_TEXT) \
  SELFTEST_BEGIN_STMT						    \
    verify_item (SELFTEST_LOCATION, (ITEM), OPTINFO_ITEM_KIND_TEXT, \
		 UNKNOWN_LOCATION, (EXPECTED_TEXT));		    \
  SELFTEST_END_STMT

/* Verify that ITEM is a tree item, with the expected values.  */

#define ASSERT_IS_TREE(ITEM, EXPECTED_LOCATION, EXPECTED_TEXT) \
  SELFTEST_BEGIN_STMT						    \
    verify_item (SELFTEST_LOCATION, (ITEM), OPTINFO_ITEM_KIND_TREE, \
		 (EXPECTED_LOCATION), (EXPECTED_TEXT));	    \
  SELFTEST_END_STMT

/* Verify that ITEM is a gimple item, with the expected values.  */

#define ASSERT_IS_GIMPLE(ITEM, EXPECTED_LOCATION, EXPECTED_TEXT) \
  SELFTEST_BEGIN_STMT						    \
    verify_item (SELFTEST_LOCATION, (ITEM), OPTINFO_ITEM_KIND_GIMPLE, \
		 (EXPECTED_LOCATION), (EXPECTED_TEXT));	    \
  SELFTEST_END_STMT

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
  location_t where = linemap_position_for_column (line_table, 10);
  if (where > LINE_MAP_MAX_LOCATION_WITH_COLS)
    return;

  dump_location_t loc = dump_location_t::from_location_t (where);

  gimple *stmt = gimple_build_return (NULL);
  gimple_set_location (stmt, where);

  tree test_decl = build_decl (UNKNOWN_LOCATION, VAR_DECL,
			       get_identifier ("test_decl"),
			       integer_type_node);
  /* Run all tests twice, with and then without optinfo enabled, to ensure
     that immediate destinations vs optinfo-based destinations both
     work, independently of each other, with no leaks.  */
  for (int i = 0 ; i < 2; i++)
    {
      bool with_optinfo = (i == 0);

      /* Test of dump_printf.  */
      {
	temp_dump_context tmp (with_optinfo, MSG_ALL);
	dump_printf (MSG_NOTE, "int: %i str: %s", 42, "foo");

	ASSERT_DUMPED_TEXT_EQ (tmp, "int: 42 str: foo");
	if (with_optinfo)
	  {
	    optinfo *info = tmp.get_pending_optinfo ();
	    ASSERT_TRUE (info != NULL);
	    ASSERT_EQ (info->get_kind (), OPTINFO_KIND_NOTE);
	    ASSERT_EQ (info->num_items (), 1);
	    ASSERT_IS_TEXT (info->get_item (0), "int: 42 str: foo");
	  }
      }

      /* Test of dump_printf with %T.  */
      {
	temp_dump_context tmp (with_optinfo, MSG_ALL);
	dump_printf (MSG_NOTE, "tree: %T", integer_zero_node);

	ASSERT_DUMPED_TEXT_EQ (tmp, "tree: 0");
	if (with_optinfo)
	  {
	    optinfo *info = tmp.get_pending_optinfo ();
	    ASSERT_TRUE (info != NULL);
	    ASSERT_EQ (info->get_kind (), OPTINFO_KIND_NOTE);
	    ASSERT_EQ (info->num_items (), 2);
	    ASSERT_IS_TEXT (info->get_item (0), "tree: ");
	    ASSERT_IS_TREE (info->get_item (1), UNKNOWN_LOCATION, "0");
	  }
      }

      /* Test of dump_printf with %E.  */
      {
	temp_dump_context tmp (with_optinfo, MSG_ALL);
	dump_printf (MSG_NOTE, "gimple: %E", stmt);

	ASSERT_DUMPED_TEXT_EQ (tmp, "gimple: return;");
	if (with_optinfo)
	  {
	    optinfo *info = tmp.get_pending_optinfo ();
	    ASSERT_TRUE (info != NULL);
	    ASSERT_EQ (info->get_kind (), OPTINFO_KIND_NOTE);
	    ASSERT_EQ (info->num_items (), 2);
	    ASSERT_IS_TEXT (info->get_item (0), "gimple: ");
	    ASSERT_IS_GIMPLE (info->get_item (1), where, "return;");
	  }
      }

      /* Test of dump_printf with %G.  */
      {
	temp_dump_context tmp (with_optinfo, MSG_ALL);
	dump_printf (MSG_NOTE, "gimple: %G", stmt);

	ASSERT_DUMPED_TEXT_EQ (tmp, "gimple: return;\n");
	if (with_optinfo)
	  {
	    optinfo *info = tmp.get_pending_optinfo ();
	    ASSERT_TRUE (info != NULL);
	    ASSERT_EQ (info->get_kind (), OPTINFO_KIND_NOTE);
	    ASSERT_EQ (info->num_items (), 2);
	    ASSERT_IS_TEXT (info->get_item (0), "gimple: ");
	    ASSERT_IS_GIMPLE (info->get_item (1), where, "return;\n");
	  }
      }

      /* dump_print_loc with multiple format codes.  This tests various
	 things:
	 - intermingling of text, format codes handled by the base
	 pretty_printer, and dump-specific format codes
	 - multiple dump-specific format codes: some consecutive, others
	 separated by text, trailing text after the final one.  */
      {
	temp_dump_context tmp (with_optinfo, MSG_ALL);
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
	    ASSERT_IS_GIMPLE (info->get_item (5), where, "return;");
	    ASSERT_IS_GIMPLE (info->get_item (6), where, "return;");
	    ASSERT_IS_TEXT (info->get_item (7), " after\n");
	  }
      }

      /* Tree, via dump_generic_expr.  */
      {
	temp_dump_context tmp (with_optinfo, MSG_ALL);
	dump_printf_loc (MSG_NOTE, loc, "test of tree: ");
	dump_generic_expr (MSG_NOTE, TDF_SLIM, integer_zero_node);

	ASSERT_DUMPED_TEXT_EQ (tmp, "test.txt:5:10: note: test of tree: 0");
	if (with_optinfo)
	  {
	    optinfo *info = tmp.get_pending_optinfo ();
	    ASSERT_TRUE (info != NULL);
	    ASSERT_EQ (info->get_location_t (), where);
	    ASSERT_EQ (info->get_kind (), OPTINFO_KIND_NOTE);
	    ASSERT_EQ (info->num_items (), 2);
	    ASSERT_IS_TEXT (info->get_item (0), "test of tree: ");
	    ASSERT_IS_TREE (info->get_item (1), UNKNOWN_LOCATION, "0");
	  }
      }

      /* Tree, via dump_generic_expr_loc.  */
      {
	temp_dump_context tmp (with_optinfo, MSG_ALL);
	dump_generic_expr_loc (MSG_NOTE, loc, TDF_SLIM, integer_one_node);

	ASSERT_DUMPED_TEXT_EQ (tmp, "test.txt:5:10: note: 1");
	if (with_optinfo)
	  {
	    optinfo *info = tmp.get_pending_optinfo ();
	    ASSERT_TRUE (info != NULL);
	    ASSERT_EQ (info->get_location_t (), where);
	    ASSERT_EQ (info->get_kind (), OPTINFO_KIND_NOTE);
	    ASSERT_EQ (info->num_items (), 1);
	    ASSERT_IS_TREE (info->get_item (0), UNKNOWN_LOCATION, "1");
	  }
      }

      /* Gimple.  */
      {
	/* dump_gimple_stmt_loc.  */
	{
	  temp_dump_context tmp (with_optinfo, MSG_ALL);
	  dump_gimple_stmt_loc (MSG_NOTE, loc, TDF_SLIM, stmt, 2);

	  ASSERT_DUMPED_TEXT_EQ (tmp, "test.txt:5:10: note: return;\n");
	  if (with_optinfo)
	    {
	      optinfo *info = tmp.get_pending_optinfo ();
	      ASSERT_TRUE (info != NULL);
	      ASSERT_EQ (info->num_items (), 1);
	      ASSERT_IS_GIMPLE (info->get_item (0), where, "return;\n");
	    }
	}

	/* dump_gimple_stmt.  */
	{
	  temp_dump_context tmp (with_optinfo, MSG_ALL);
	  dump_gimple_stmt (MSG_NOTE, TDF_SLIM, stmt, 2);

	  ASSERT_DUMPED_TEXT_EQ (tmp, "return;\n");
	  if (with_optinfo)
	    {
	      optinfo *info = tmp.get_pending_optinfo ();
	      ASSERT_TRUE (info != NULL);
	      ASSERT_EQ (info->num_items (), 1);
	      ASSERT_IS_GIMPLE (info->get_item (0), where, "return;\n");
	    }
	}

	/* dump_gimple_expr_loc.  */
	{
	  temp_dump_context tmp (with_optinfo, MSG_ALL);
	  dump_gimple_expr_loc (MSG_NOTE, loc, TDF_SLIM, stmt, 2);

	  ASSERT_DUMPED_TEXT_EQ (tmp, "test.txt:5:10: note: return;");
	  if (with_optinfo)
	    {
	      optinfo *info = tmp.get_pending_optinfo ();
	      ASSERT_TRUE (info != NULL);
	      ASSERT_EQ (info->num_items (), 1);
	      ASSERT_IS_GIMPLE (info->get_item (0), where, "return;");
	    }
	}

	/* dump_gimple_expr.  */
	{
	  temp_dump_context tmp (with_optinfo, MSG_ALL);
	  dump_gimple_expr (MSG_NOTE, TDF_SLIM, stmt, 2);

	  ASSERT_DUMPED_TEXT_EQ (tmp, "return;");
	  if (with_optinfo)
	    {
	      optinfo *info = tmp.get_pending_optinfo ();
	      ASSERT_TRUE (info != NULL);
	      ASSERT_EQ (info->num_items (), 1);
	      ASSERT_IS_GIMPLE (info->get_item (0), where, "return;");
	    }
	}
      }

      /* poly_int.  */
      {
	temp_dump_context tmp (with_optinfo, MSG_ALL);
	dump_dec (MSG_NOTE, poly_int64 (42));

	ASSERT_DUMPED_TEXT_EQ (tmp, "42");
	if (with_optinfo)
	  {
	    optinfo *info = tmp.get_pending_optinfo ();
	    ASSERT_TRUE (info != NULL);
	    ASSERT_EQ (info->num_items (), 1);
	    ASSERT_IS_TEXT (info->get_item (0), "42");
	  }
      }

      /* scopes.  */
      {
	temp_dump_context tmp (with_optinfo, MSG_ALL);
	dump_printf_loc (MSG_NOTE, stmt, "msg 1\n");
	{
	  AUTO_DUMP_SCOPE ("outer scope", stmt);
	  dump_printf_loc (MSG_NOTE, stmt, "msg 2\n");
	  {
	    AUTO_DUMP_SCOPE ("middle scope", stmt);
	    dump_printf_loc (MSG_NOTE, stmt, "msg 3\n");
	    {
	      AUTO_DUMP_SCOPE ("inner scope", stmt);
	      dump_printf_loc (MSG_NOTE, stmt, "msg 4\n");
	    }
	    dump_printf_loc (MSG_NOTE, stmt, "msg 5\n");
	  }
	  dump_printf_loc (MSG_NOTE, stmt, "msg 6\n");
	}
	dump_printf_loc (MSG_NOTE, stmt, "msg 7\n");

	ASSERT_DUMPED_TEXT_EQ (tmp,
			       "test.txt:5:10: note: msg 1\n"
			       "test.txt:5:10: note: === outer scope ===\n"
			       "test.txt:5:10: note:  msg 2\n"
			       "test.txt:5:10: note:  === middle scope ===\n"
			       "test.txt:5:10: note:   msg 3\n"
			       "test.txt:5:10: note:   === inner scope ===\n"
			       "test.txt:5:10: note:    msg 4\n"
			       "test.txt:5:10: note:   msg 5\n"
			       "test.txt:5:10: note:  msg 6\n"
			       "test.txt:5:10: note: msg 7\n");
	if (with_optinfo)
	  {
	    optinfo *info = tmp.get_pending_optinfo ();
	    ASSERT_TRUE (info != NULL);
	    ASSERT_EQ (info->num_items (), 1);
	    ASSERT_IS_TEXT (info->get_item (0), "msg 7\n");
	  }
      }
    }

  /* Verify that MSG_* affects optinfo->get_kind (); we tested MSG_NOTE
     above.  */
  {
    /* MSG_OPTIMIZED_LOCATIONS.  */
    {
      temp_dump_context tmp (true, MSG_ALL);
      dump_printf_loc (MSG_OPTIMIZED_LOCATIONS, loc, "test");
      ASSERT_EQ (tmp.get_pending_optinfo ()->get_kind (),
		 OPTINFO_KIND_SUCCESS);
    }

    /* MSG_MISSED_OPTIMIZATION.  */
    {
      temp_dump_context tmp (true, MSG_ALL);
      dump_printf_loc (MSG_MISSED_OPTIMIZATION, loc, "test");
      ASSERT_EQ (tmp.get_pending_optinfo ()->get_kind (),
		 OPTINFO_KIND_FAILURE);
    }
  }
}

/* Run all of the selftests within this file.  */

void
dumpfile_c_tests ()
{
  test_impl_location ();
  for_each_line_table_case (test_capture_of_dump_calls);
}

} // namespace selftest

#endif /* CHECKING_P */
