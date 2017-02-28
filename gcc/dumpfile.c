/* Dump infrastructure for optimizations and intermediate representation.
   Copyright (C) 2012-2017 Free Software Foundation, Inc.

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

/* If non-NULL, return one past-the-end of the matching SUBPART of
   the WHOLE string.  */
#define skip_leading_substring(whole,  part) \
   (strncmp (whole, part, strlen (part)) ? NULL : whole + strlen (part))

static int pflags;                   /* current dump_flags */
static int alt_flags;                /* current opt_info flags */

static void dump_loc (int, FILE *, source_location);
static FILE *dump_open_alternate_stream (struct dump_file_info *);

/* These are currently used for communicating between passes.
   However, instead of accessing them directly, the passes can use
   dump_printf () for dumps.  */
FILE *dump_file = NULL;
FILE *alt_dump_file = NULL;
const char *dump_file_name;
int dump_flags;

/* Table of tree dump switches. This must be consistent with the
   TREE_DUMP_INDEX enumeration in dumpfile.h.  */
static struct dump_file_info dump_files[TDI_end] =
{
  {NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0, 0, 0, 0, 0, 0, false, false},
  {".cgraph", "ipa-cgraph", NULL, NULL, NULL, NULL, NULL, TDF_IPA,
   0, 0, 0, 0, 0, false, false},
  {".type-inheritance", "ipa-type-inheritance", NULL, NULL, NULL, NULL, NULL, TDF_IPA,
   0, 0, 0, 0, 0, false, false},
  {".ipa-clones", "ipa-clones", NULL, NULL, NULL, NULL, NULL, TDF_IPA,
   0, 0, 0, 0, 0, false, false},
  {".tu", "translation-unit", NULL, NULL, NULL, NULL, NULL, TDF_TREE,
   0, 0, 0, 0, 1, false, false},
  {".class", "class-hierarchy", NULL, NULL, NULL, NULL, NULL, TDF_TREE,
   0, 0, 0, 0, 2, false, false},
  {".original", "tree-original", NULL, NULL, NULL, NULL, NULL, TDF_TREE,
   0, 0, 0, 0, 3, false, false},
  {".gimple", "tree-gimple", NULL, NULL, NULL, NULL, NULL, TDF_TREE,
   0, 0, 0, 0, 4, false, false},
  {".nested", "tree-nested", NULL, NULL, NULL, NULL, NULL, TDF_TREE,
   0, 0, 0, 0, 5, false, false},
#define FIRST_AUTO_NUMBERED_DUMP 6

  {NULL, "tree-all", NULL, NULL, NULL, NULL, NULL, TDF_TREE,
   0, 0, 0, 0, 0, false, false},
  {NULL, "rtl-all", NULL, NULL, NULL, NULL, NULL, TDF_RTL,
   0, 0, 0, 0, 0, false, false},
  {NULL, "ipa-all", NULL, NULL, NULL, NULL, NULL, TDF_IPA,
   0, 0, 0, 0, 0, false, false},
};

/* Define a name->number mapping for a dump flag value.  */
struct dump_option_value_info
{
  const char *const name;	/* the name of the value */
  const int value;		/* the value of the name */
};

/* Table of dump options. This must be consistent with the TDF_* flags
   in dumpfile.h and opt_info_options below. */
static const struct dump_option_value_info dump_options[] =
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
  {"verbose", TDF_VERBOSE},
  {"eh", TDF_EH},
  {"alias", TDF_ALIAS},
  {"nouid", TDF_NOUID},
  {"enumerate_locals", TDF_ENUMERATE_LOCALS},
  {"scev", TDF_SCEV},
  {"gimple", TDF_GIMPLE},
  {"optimized", MSG_OPTIMIZED_LOCATIONS},
  {"missed", MSG_MISSED_OPTIMIZATION},
  {"note", MSG_NOTE},
  {"optall", MSG_ALL},
  {"all", ~(TDF_RAW | TDF_SLIM | TDF_LINENO | TDF_TREE | TDF_RTL | TDF_IPA
	    | TDF_STMTADDR | TDF_GRAPH | TDF_DIAGNOSTIC | TDF_VERBOSE
	    | TDF_RHS_ONLY | TDF_NOUID | TDF_ENUMERATE_LOCALS | TDF_SCEV
	    | TDF_GIMPLE)},
  {NULL, 0}
};

/* A subset of the dump_options table which is used for -fopt-info
   types. This must be consistent with the MSG_* flags in dumpfile.h.
 */
static const struct dump_option_value_info optinfo_verbosity_options[] =
{
  {"optimized", MSG_OPTIMIZED_LOCATIONS},
  {"missed", MSG_MISSED_OPTIMIZATION},
  {"note", MSG_NOTE},
  {"all", MSG_ALL},
  {NULL, 0}
};

/* Flags used for -fopt-info groups.  */
static const struct dump_option_value_info optgroup_options[] =
{
  {"ipa", OPTGROUP_IPA},
  {"loop", OPTGROUP_LOOP},
  {"inline", OPTGROUP_INLINE},
  {"omp", OPTGROUP_OMP},
  {"vec", OPTGROUP_VEC},
  {"optall", OPTGROUP_ALL},
  {NULL, 0}
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
	       int flags, int optgroup_flags,
	       bool take_ownership)
{
  int num = m_next_dump++;

  size_t count = m_extra_dump_files_in_use++;

  if (count >= m_extra_dump_files_alloced)
    {
      if (m_extra_dump_files_alloced == 0)
	m_extra_dump_files_alloced = 32;
      else
	m_extra_dump_files_alloced *= 2;
      m_extra_dump_files = XRESIZEVEC (struct dump_file_info,
				       m_extra_dump_files,
				       m_extra_dump_files_alloced);
    }

  memset (&m_extra_dump_files[count], 0, sizeof (struct dump_file_info));
  m_extra_dump_files[count].suffix = suffix;
  m_extra_dump_files[count].swtch = swtch;
  m_extra_dump_files[count].glob = glob;
  m_extra_dump_files[count].pflags = flags;
  m_extra_dump_files[count].optgroup_flags = optgroup_flags;
  m_extra_dump_files[count].num = num;
  m_extra_dump_files[count].owns_strings = take_ownership;

  return count + TDI_end;
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
    if (0 == strcmp (m_extra_dump_files[i].swtch, swtch))
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
get_dump_file_name (int phase) const
{
  struct dump_file_info *dfi;

  if (phase == TDI_none)
    return NULL;

  dfi = get_dump_file_info (phase);

  return get_dump_file_name (dfi);
}

/* Return the name of the dump file for the given dump_file_info.
   The caller is responsible for calling free on the returned
   buffer.
   If the dump is not enabled, returns NULL.  */

char *
gcc::dump_manager::
get_dump_file_name (struct dump_file_info *dfi) const
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
      char suffix;
      if (dfi->pflags & TDF_TREE)
	suffix = 't';
      else if (dfi->pflags & TDF_IPA)
	suffix = 'i';
      else
	suffix = 'r';

      if (snprintf (dump_id, sizeof (dump_id), ".%03d%c", dfi->num, suffix) < 0)
	dump_id[0] = '\0';
    }

  return concat (dump_base_name, dump_id, dfi->suffix, NULL);
}

/* For a given DFI, open an alternate dump filename (which could also
   be a standard stream such as stdout/stderr). If the alternate dump
   file cannot be opened, return NULL.  */

static FILE *
dump_open_alternate_stream (struct dump_file_info *dfi)
{
  FILE *stream ;
  if (!dfi->alt_filename)
    return NULL;

  if (dfi->alt_stream)
    return dfi->alt_stream;

  stream = strcmp ("stderr", dfi->alt_filename) == 0
    ? stderr
    : strcmp ("stdout", dfi->alt_filename) == 0
    ? stdout
    : fopen (dfi->alt_filename, dfi->alt_state < 0 ? "w" : "a");

  if (!stream)
    error ("could not open dump file %qs: %m", dfi->alt_filename);
  else
    dfi->alt_state = 1;

  return stream;
}

/* Print source location on DFILE if enabled.  */

void
dump_loc (int dump_kind, FILE *dfile, source_location loc)
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
    }
}

/* Dump gimple statement GS with SPC indentation spaces and
   EXTRA_DUMP_FLAGS on the dump streams if DUMP_KIND is enabled.  */

void
dump_gimple_stmt (int dump_kind, int extra_dump_flags, gimple *gs, int spc)
{
  if (dump_file && (dump_kind & pflags))
    print_gimple_stmt (dump_file, gs, spc, dump_flags | extra_dump_flags);

  if (alt_dump_file && (dump_kind & alt_flags))
    print_gimple_stmt (alt_dump_file, gs, spc, dump_flags | extra_dump_flags);
}

/* Similar to dump_gimple_stmt, except additionally print source location.  */

void
dump_gimple_stmt_loc (int dump_kind, source_location loc, int extra_dump_flags,
		      gimple *gs, int spc)
{
  if (dump_file && (dump_kind & pflags))
    {
      dump_loc (dump_kind, dump_file, loc);
      print_gimple_stmt (dump_file, gs, spc, dump_flags | extra_dump_flags);
    }

  if (alt_dump_file && (dump_kind & alt_flags))
    {
      dump_loc (dump_kind, alt_dump_file, loc);
      print_gimple_stmt (alt_dump_file, gs, spc, dump_flags | extra_dump_flags);
    }
}

/* Dump expression tree T using EXTRA_DUMP_FLAGS on dump streams if
   DUMP_KIND is enabled.  */

void
dump_generic_expr (int dump_kind, int extra_dump_flags, tree t)
{
  if (dump_file && (dump_kind & pflags))
      print_generic_expr (dump_file, t, dump_flags | extra_dump_flags);

  if (alt_dump_file && (dump_kind & alt_flags))
      print_generic_expr (alt_dump_file, t, dump_flags | extra_dump_flags);
}


/* Similar to dump_generic_expr, except additionally print the source
   location.  */

void
dump_generic_expr_loc (int dump_kind, source_location loc,
                       int extra_dump_flags, tree t)
{
  if (dump_file && (dump_kind & pflags))
    {
      dump_loc (dump_kind, dump_file, loc);
      print_generic_expr (dump_file, t, dump_flags | extra_dump_flags);
    }

  if (alt_dump_file && (dump_kind & alt_flags))
    {
      dump_loc (dump_kind, alt_dump_file, loc);
      print_generic_expr (alt_dump_file, t, dump_flags | extra_dump_flags);
    }
}

/* Output a formatted message using FORMAT on appropriate dump streams.  */

void
dump_printf (int dump_kind, const char *format, ...)
{
  if (dump_file && (dump_kind & pflags))
    {
      va_list ap;
      va_start (ap, format);
      vfprintf (dump_file, format, ap);
      va_end (ap);
    }

  if (alt_dump_file && (dump_kind & alt_flags))
    {
      va_list ap;
      va_start (ap, format);
      vfprintf (alt_dump_file, format, ap);
      va_end (ap);
    }
}

/* Similar to dump_printf, except source location is also printed.  */

void
dump_printf_loc (int dump_kind, source_location loc, const char *format, ...)
{
  if (dump_file && (dump_kind & pflags))
    {
      va_list ap;
      dump_loc (dump_kind, dump_file, loc);
      va_start (ap, format);
      vfprintf (dump_file, format, ap);
      va_end (ap);
    }

  if (alt_dump_file && (dump_kind & alt_flags))
    {
      va_list ap;
      dump_loc (dump_kind, alt_dump_file, loc);
      va_start (ap, format);
      vfprintf (alt_dump_file, format, ap);
      va_end (ap);
    }
}

/* Start a dump for PHASE. Store user-supplied dump flags in
   *FLAG_PTR.  Return the number of streams opened.  Set globals
   DUMP_FILE, and ALT_DUMP_FILE to point to the opened streams, and
   set dump_flags appropriately for both pass dump stream and
   -fopt-info stream. */

int
gcc::dump_manager::
dump_start (int phase, int *flag_ptr)
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
      stream = strcmp ("stderr", name) == 0
          ? stderr
          : strcmp ("stdout", name) == 0
          ? stdout
          : fopen (name, dfi->pstate < 0 ? "w" : "a");
      if (!stream)
        error ("could not open dump file %qs: %m", name);
      else
        {
          dfi->pstate = 1;
          count++;
        }
      free (name);
      dfi->pstream = stream;
      dump_file = dfi->pstream;
      /* Initialize current dump flags. */
      pflags = dfi->pflags;
    }

  stream = dump_open_alternate_stream (dfi);
  if (stream)
    {
      dfi->alt_stream = stream;
      count++;
      alt_dump_file = dfi->alt_stream;
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
  if (dfi->pstream && (!dfi->pfilename
                       || (strcmp ("stderr", dfi->pfilename) != 0
                           && strcmp ("stdout", dfi->pfilename) != 0)))
    fclose (dfi->pstream);

  if (dfi->alt_stream && strcmp ("stderr", dfi->alt_filename) != 0
      && strcmp ("stdout", dfi->alt_filename) != 0)
    fclose (dfi->alt_stream);

  dfi->alt_stream = NULL;
  dfi->pstream = NULL;
  dump_file = NULL;
  alt_dump_file = NULL;
  dump_flags = TDI_none;
  alt_flags = 0;
  pflags = 0;
}

/* Begin a tree dump for PHASE. Stores any user supplied flag in
   *FLAG_PTR and returns a stream to write to. If the dump is not
   enabled, returns NULL.
   Multiple calls will reopen and append to the dump file.  */

FILE *
dump_begin (int phase, int *flag_ptr)
{
  return g->get_dumps ()->dump_begin (phase, flag_ptr);
}

FILE *
gcc::dump_manager::
dump_begin (int phase, int *flag_ptr)
{
  char *name;
  struct dump_file_info *dfi;
  FILE *stream;

  if (phase == TDI_none || !dump_phase_enabled_p (phase))
    return NULL;

  name = get_dump_file_name (phase);
  if (!name)
    return NULL;
  dfi = get_dump_file_info (phase);

  stream = strcmp ("stderr", name) == 0
    ? stderr
    : strcmp ("stdout", name) == 0
    ? stdout
    : fopen (name, dfi->pstate < 0 ? "w" : "a");

  if (!stream)
    error ("could not open dump file %qs: %m", name);
  else
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
dump_enable_all (int flags, const char *filename)
{
  int ir_dump_type = (flags & (TDF_TREE | TDF_RTL | TDF_IPA));
  int n = 0;
  size_t i;

  for (i = TDI_none + 1; i < (size_t) TDI_end; i++)
    {
      if ((dump_files[i].pflags & ir_dump_type))
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
      if ((m_extra_dump_files[i].pflags & ir_dump_type))
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
opt_info_enable_passes (int optgroup_flags, int flags, const char *filename)
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
  int flags;

  if (doglob && !dfi->glob)
    return 0;

  option_value = skip_leading_substring (arg, doglob ? dfi->glob : dfi->swtch);
  if (!option_value)
    return 0;

  if (*option_value && *option_value != '-' && *option_value != '=')
    return 0;

  ptr = option_value;
  flags = 0;

  while (*ptr)
    {
      const struct dump_option_value_info *option_ptr;
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
    dump_enable_all (dfi->pflags, dfi->pfilename);

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
opt_info_switch_p_1 (const char *arg, int *flags, int *optgroup_flags,
                     char **filename)
{
  const char *option_value;
  const char *ptr;

  option_value = arg;
  ptr = option_value;

  *filename = NULL;
  *flags = 0;
  *optgroup_flags = 0;

  if (!ptr)
    return 1;       /* Handle '-fopt-info' without any additional options.  */

  while (*ptr)
    {
      const struct dump_option_value_info *option_ptr;
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

      for (option_ptr = optinfo_verbosity_options; option_ptr->name;
           option_ptr++)
	if (strlen (option_ptr->name) == length
	    && !memcmp (option_ptr->name, ptr, length))
          {
            *flags |= option_ptr->value;
	    goto found;
          }

      for (option_ptr = optgroup_options; option_ptr->name; option_ptr++)
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
  int flags;
  int optgroup_flags;
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
dump_basic_block (int dump_kind, basic_block bb, int indent)
{
  if (dump_file && (dump_kind & pflags))
    dump_bb (dump_file, bb, indent, TDF_DETAILS);
  if (alt_dump_file && (dump_kind & alt_flags))
    dump_bb (alt_dump_file, bb, indent, TDF_DETAILS);
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
    dumps->dump_enable_all (TDF_RTL | TDF_DETAILS | TDF_BLOCKS, NULL);
  return num_enabled > 0;
}
