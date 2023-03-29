/* Precompiled header implementation for the C languages.
   Copyright (C) 2000-2023 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "target.h"
#include "c-common.h"
#include "timevar.h"
#include "flags.h"
#include "debug.h"
#include "c-pragma.h"
#include "langhooks.h"
#include "hosthooks.h"

/* This is a list of flag variables that must match exactly, and their
   names for the error message.  The possible values for *flag_var must
   fit in a 'signed char'.  */

static const struct c_pch_matching
{
  int *flag_var;
  const char *flag_name;
} pch_matching[] = {
  { &flag_exceptions, "-fexceptions" },
};

enum {
  MATCH_SIZE = ARRAY_SIZE (pch_matching)
};

/* Information about flags and suchlike that affect PCH validity.

   Before this structure is read, both an initial 8-character identification
   string, and a 16-byte checksum, have been read and validated.  */

struct c_pch_validity
{
  uint32_t pch_write_symbols;
  signed char match[MATCH_SIZE];
  size_t target_data_length;
};

#define IDENT_LENGTH 8

/* The file we'll be writing the PCH to.  */
static FILE *pch_outfile;

static const char *get_ident (void);

/* Compute an appropriate 8-byte magic number for the PCH file, so that
   utilities like file(1) can identify it, and so that GCC can quickly
   ignore non-PCH files and PCH files that are of a completely different
   format.  */

static const char *
get_ident (void)
{
  static char result[IDENT_LENGTH];
  static const char templ[] = "gpch.014";
  static const char c_language_chars[] = "Co+O";

  memcpy (result, templ, IDENT_LENGTH);
  result[4] = c_language_chars[c_language];

  return result;
}

/* Whether preprocessor state should be saved by pch_init.  */

static bool pch_ready_to_save_cpp_state = false;

/* Prepare to write a PCH file, if one is being written.  This is
   called at the start of compilation.  */

void
pch_init (void)
{
  FILE *f;
  struct c_pch_validity v;
  void *target_validity;
  static const char partial_pch[] = "gpcWrite";

  if (!pch_file)
    return;

  f = fopen (pch_file, "w+b");
  if (f == NULL)
    fatal_error (input_location, "cannot create precompiled header %s: %m",
		 pch_file);
  pch_outfile = f;

  memset (&v, '\0', sizeof (v));
  v.pch_write_symbols = write_symbols;
  {
    size_t i;
    for (i = 0; i < MATCH_SIZE; i++)
      {
	v.match[i] = *pch_matching[i].flag_var;
	gcc_assert (v.match[i] == *pch_matching[i].flag_var);
      }
  }
  target_validity = targetm.get_pch_validity (&v.target_data_length);

  if (fwrite (partial_pch, IDENT_LENGTH, 1, f) != 1
      || fwrite (executable_checksum, 16, 1, f) != 1
      || fwrite (&v, sizeof (v), 1, f) != 1
      || fwrite (target_validity, v.target_data_length, 1, f) != 1)
    fatal_error (input_location, "cannot write to %s: %m", pch_file);

  /* Let the debugging format deal with the PCHness.  */
  (*debug_hooks->handle_pch) (0);

  if (pch_ready_to_save_cpp_state)
    pch_cpp_save_state ();

  XDELETE (target_validity);
}

/* Whether preprocessor state has been saved in a PCH file.  */

static bool pch_cpp_state_saved = false;

/* Save preprocessor state in a PCH file, after implicitly included
   headers have been read.  If the PCH file has not yet been opened,
   record that state should be saved when it is opened.  */

void
pch_cpp_save_state (void)
{
  if (!pch_cpp_state_saved)
    {
      if (pch_outfile)
	{
	  cpp_save_state (parse_in, pch_outfile);
	  pch_cpp_state_saved = true;
	}
      else
	pch_ready_to_save_cpp_state = true;
    }
}

/* Write the PCH file.  This is called at the end of a compilation which
   will produce a PCH file.  */

void
c_common_write_pch (void)
{
  timevar_push (TV_PCH_SAVE);

  targetm.prepare_pch_save ();

  (*debug_hooks->handle_pch) (1);

  prepare_target_option_nodes_for_pch ();

  cpp_write_pch_deps (parse_in, pch_outfile);

  gt_pch_save (pch_outfile);

  timevar_push (TV_PCH_CPP_SAVE);
  cpp_write_pch_state (parse_in, pch_outfile);
  timevar_pop (TV_PCH_CPP_SAVE);

  if (fseek (pch_outfile, 0, SEEK_SET) != 0
      || fwrite (get_ident (), IDENT_LENGTH, 1, pch_outfile) != 1)
    fatal_error (input_location, "cannot write %s: %m", pch_file);

  fclose (pch_outfile);

  timevar_pop (TV_PCH_SAVE);
}

/* Check the PCH file called NAME, open on FD, to see if it can be
   used in this compilation.  Return 1 if valid, 0 if the file can't
   be used now but might be if it's seen later in the compilation, and
   2 if this file could never be used in the compilation.  */

int
c_common_valid_pch (cpp_reader *pfile, const char *name, int fd)
{
  int sizeread;
  int result;
  char ident[IDENT_LENGTH + 16];
  const char *pch_ident;
  struct c_pch_validity v;

  /* Perform a quick test of whether this is a valid
     precompiled header for the current language.  */

  /* C++ modules and PCH don't play together.  */
  if (flag_modules)
    return 2;

  sizeread = read (fd, ident, IDENT_LENGTH + 16);
  if (sizeread == -1)
    fatal_error (input_location, "cannot read %s: %m", name);
  else if (sizeread != IDENT_LENGTH + 16)
    {
      cpp_warning (pfile, CPP_W_INVALID_PCH, "%s: too short to be a PCH file",
		   name);
      return 2;
    }

  pch_ident = get_ident();
  if (memcmp (ident, pch_ident, IDENT_LENGTH) != 0)
    {
	if (memcmp (ident, pch_ident, 5) == 0)
	  /* It's a PCH, for the right language, but has the wrong version.  */
	  cpp_warning (pfile, CPP_W_INVALID_PCH,
		       "%s: not compatible with this GCC version", name);
	else if (memcmp (ident, pch_ident, 4) == 0)
	  /* It's a PCH for the wrong language.  */
	  cpp_warning (pfile, CPP_W_INVALID_PCH, "%s: not for %s", name,
		       lang_hooks.name);
	else
	  /* Not any kind of PCH.  */
	  cpp_warning (pfile, CPP_W_INVALID_PCH, "%s: not a PCH file", name);
      return 2;
    }
  if (memcmp (ident + IDENT_LENGTH, executable_checksum, 16) != 0)
    {
      cpp_warning (pfile, CPP_W_INVALID_PCH,
		   "%s: created by a different GCC executable", name);
      return 2;
    }

  /* At this point, we know it's a PCH file created by this
     executable, so it ought to be long enough that we can read a
     c_pch_validity structure.  */
  if (read (fd, &v, sizeof (v)) != sizeof (v))
    fatal_error (input_location, "cannot read %s: %m", name);

  /* The allowable debug info combinations are that either the PCH file
     was built with the same as is being used now, or the PCH file was
     built for some kind of debug info but now none is in use.  */
  if (v.pch_write_symbols != write_symbols
      && write_symbols != NO_DEBUG)
    {
      char *created_str = xstrdup (debug_set_names (v.pch_write_symbols));
      char *used_str = xstrdup (debug_set_names (write_symbols));
      cpp_warning (pfile, CPP_W_INVALID_PCH,
		   "%s: created with '%s' debug info, but used with '%s'", name,
		   created_str, used_str);
      free (created_str);
      free (used_str);
      return 2;
    }

  /* Check flags that must match exactly.  */
  {
    size_t i;
    for (i = 0; i < MATCH_SIZE; i++)
      if (*pch_matching[i].flag_var != v.match[i])
	{
	  cpp_warning (pfile, CPP_W_INVALID_PCH,
		       "%s: settings for %s do not match", name,
		       pch_matching[i].flag_name);
	  return 2;
	}
  }

  /* Check the target-specific validity data.  */
  {
    void *this_file_data = xmalloc (v.target_data_length);
    const char *msg;

    if ((size_t) read (fd, this_file_data, v.target_data_length)
	!= v.target_data_length)
      fatal_error (input_location, "cannot read %s: %m", name);
    msg = targetm.pch_valid_p (this_file_data, v.target_data_length);
    free (this_file_data);
    if (msg != NULL)
      {
	cpp_warning (pfile, CPP_W_INVALID_PCH, "%s: %s", name, msg);
	return 2;
      }
  }

  /* Check the preprocessor macros are the same as when the PCH was
     generated.  */

  result = cpp_valid_state (pfile, name, fd);
  if (result == -1)
    return 2;
  else
    return result == 0;
}

/* If non-NULL, this function is called after a precompile header file
   is loaded.  */
void (*lang_post_pch_load) (void);

/* Load in the PCH file NAME, open on FD.  It was originally searched for
   by ORIG_NAME.  */

void
c_common_read_pch (cpp_reader *pfile, const char *name,
		   int fd, const char *orig_name ATTRIBUTE_UNUSED)
{
  FILE *f;
  struct save_macro_data *smd;
  expanded_location saved_loc;
  bool saved_trace_includes;

  timevar_push (TV_PCH_RESTORE);

  f = fdopen (fd, "rb");
  if (f == NULL)
    {
      cpp_errno (pfile, CPP_DL_ERROR, "calling fdopen");
      close (fd);
      goto end;
    }

  cpp_get_callbacks (parse_in)->valid_pch = NULL;

  /* Save the location and then restore it after reading the PCH.  */
  saved_loc = expand_location (line_table->highest_line);
  saved_trace_includes = line_table->trace_includes;

  timevar_push (TV_PCH_CPP_RESTORE);
  cpp_prepare_state (pfile, &smd);
  timevar_pop (TV_PCH_CPP_RESTORE);

  gt_pch_restore (f);
  cpp_set_line_map (pfile, line_table);
  rebuild_location_adhoc_htab (line_table);

  timevar_push (TV_PCH_CPP_RESTORE);
  if (cpp_read_state (pfile, name, f, smd) != 0)
    {
      fclose (f);
      timevar_pop (TV_PCH_CPP_RESTORE);
      goto end;
    }
  timevar_pop (TV_PCH_CPP_RESTORE);


  fclose (f);

  line_table->trace_includes = saved_trace_includes;
  linemap_add (line_table, LC_ENTER, 0, saved_loc.file, saved_loc.line);

  /* Give the front end a chance to take action after a PCH file has
     been loaded.  */
  if (lang_post_pch_load)
    (*lang_post_pch_load) ();

end:
  timevar_pop (TV_PCH_RESTORE);
}

/* Indicate that no more PCH files should be read.  */

void
c_common_no_more_pch (void)
{
  if (cpp_get_callbacks (parse_in)->valid_pch)
    {
      cpp_get_callbacks (parse_in)->valid_pch = NULL;
      void *addr = NULL;
      host_hooks.gt_pch_use_address (addr, 0, -1, 0);
    }
}

/* Handle #pragma GCC pch_preprocess, to load in the PCH file.  */

void
c_common_pch_pragma (cpp_reader *pfile, const char *name)
{
  int fd;

  if (!cpp_get_options (pfile)->preprocessed)
    {
      error ("%<pch_preprocess%> pragma should only be used "
	     "with %<-fpreprocessed%>");
      inform (input_location, "use %<#include%> instead");
      return;
    }

  fd = open (name, O_RDONLY | O_BINARY, 0666);
  if (fd == -1)
    fatal_error (input_location, "%s: couldn%'t open PCH file: %m", name);

  if (c_common_valid_pch (pfile, name, fd) != 1)
    {
      if (!cpp_get_options (pfile)->warn_invalid_pch)
	inform (input_location, "use %<-Winvalid-pch%> for more information");
      fatal_error (input_location, "%s: PCH file was invalid", name);
    }

  c_common_read_pch (pfile, name, fd, name);

  close (fd);
}

