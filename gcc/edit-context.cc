/* Determining the results of applying fix-it hints.
   Copyright (C) 2016-2023 Free Software Foundation, Inc.

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
#include "line-map.h"
#include "edit-context.h"
#include "pretty-print.h"
#include "diagnostic-color.h"
#include "selftest.h"

/* This file implements a way to track the effect of fix-its,
   via a class edit_context; the other classes are support classes for
   edit_context.

   A complication here is that fix-its are expressed relative to coordinates
   in the file when it was parsed, before any changes have been made, and
   so if there's more that one fix-it to be applied, we have to adjust
   later fix-its to allow for the changes made by earlier ones.  This
   is done by the various "get_effective_column" methods.

   The "filename" params are required to outlive the edit_context (no
   copy of the underlying str is taken, just the ptr).  */

/* Forward decls.  class edit_context is declared within edit-context.h.
   The other types are declared here.  */
class edit_context;
class edited_file;
class edited_line;
class line_event;

/* A struct to hold the params of a print_diff call.  */

class diff
{
public:
  diff (pretty_printer *pp, bool show_filenames)
  : m_pp (pp), m_show_filenames (show_filenames) {}

  pretty_printer *m_pp;
  bool m_show_filenames;
};

/* The state of one named file within an edit_context: the filename,
   and the lines that have been edited so far.  */

class edited_file
{
 public:
  edited_file (const char *filename);
  static void delete_cb (edited_file *file);

  const char *get_filename () const { return m_filename; }
  char *get_content ();

  bool apply_fixit (int line, int start_column,
		    int next_column,
		    const char *replacement_str,
		    int replacement_len);
  int get_effective_column (int line, int column);

  static int call_print_diff (const char *, edited_file *file,
			      void *user_data)
  {
    diff *d = (diff *)user_data;
    file->print_diff (d->m_pp, d->m_show_filenames);
    return 0;
  }

 private:
  bool print_content (pretty_printer *pp);
  void print_diff (pretty_printer *pp, bool show_filenames);
  int print_diff_hunk (pretty_printer *pp, int old_start_of_hunk,
		       int old_end_of_hunk, int new_start_of_hunk);
  edited_line *get_line (int line);
  edited_line *get_or_insert_line (int line);
  int get_num_lines (bool *missing_trailing_newline);

  int get_effective_line_count (int old_start_of_hunk,
				int old_end_of_hunk);

  void print_run_of_changed_lines (pretty_printer *pp,
				   int start_of_run,
				   int end_of_run);

  const char *m_filename;
  typed_splay_tree<int, edited_line *> m_edited_lines;
  int m_num_lines;
};

/* A line added before an edited_line.  */

class added_line
{
 public:
  added_line (const char *content, int len)
  : m_content (xstrndup (content, len)), m_len (len) {}
  ~added_line () { free (m_content); }

  const char *get_content () const { return m_content; }
  int get_len () const { return m_len; }

 private:
  char *m_content;
  int m_len;
};

/* The state of one edited line within an edited_file.
   As well as the current content of the line, it contains a record of
   the changes, so that further changes can be applied in the correct
   place.

   When handling fix-it hints containing newlines, new lines are added
   as added_line predecessors to an edited_line.  Hence it's possible
   for an "edited_line" to not actually have been changed, but to merely
   be a placeholder for the lines added before it.  This can be tested
   for with actuall_edited_p, and has a slight effect on how diff hunks
   are generated.  */

class edited_line
{
 public:
  edited_line (const char *filename, int line_num);
  ~edited_line ();
  static void delete_cb (edited_line *el);

  int get_line_num () const { return m_line_num; }
  const char *get_content () const { return m_content; }
  int get_len () const { return m_len; }

  int get_effective_column (int orig_column) const;
  bool apply_fixit (int start_column,
		    int next_column,
		    const char *replacement_str,
		    int replacement_len);

  int get_effective_line_count () const;

  /* Has the content of this line actually changed, or are we merely
     recording predecessor added_lines?  */
  bool actually_edited_p () const { return m_line_events.length () > 0; }

  void print_content (pretty_printer *pp) const;
  void print_diff_lines (pretty_printer *pp) const;

 private:
  void ensure_capacity (int len);
  void ensure_terminated ();

  int m_line_num;
  char *m_content;
  int m_len;
  int m_alloc_sz;
  auto_vec <line_event> m_line_events;
  auto_vec <added_line *> m_predecessors;
};

/* Class for representing edit events that have occurred on one line of
   one file: the replacement of some text betweeen some columns
   on the line.

   Subsequent events will need their columns adjusting if they're
   are on this line and their column is >= the start point.  */

class line_event
{
 public:
  line_event (int start, int next, int len) : m_start (start),
    m_delta (len - (next - start)) {}

  int get_effective_column (int orig_column) const
  {
    if (orig_column >= m_start)
      return orig_column += m_delta;
    else
      return orig_column;
  }

 private:
  int m_start;
  int m_delta;
};

/* Forward decls.  */

static void
print_diff_line (pretty_printer *pp, char prefix_char,
		 const char *line, int line_size);

/* Implementation of class edit_context.  */

/* edit_context's ctor.  */

edit_context::edit_context ()
: m_valid (true),
  m_files (strcmp, NULL, edited_file::delete_cb)
{}

/* Add any fixits within RICHLOC to this context, recording the
   changes that they make.  */

void
edit_context::add_fixits (rich_location *richloc)
{
  if (!m_valid)
    return;
  if (richloc->seen_impossible_fixit_p ())
    {
      m_valid = false;
      return;
    }
  for (unsigned i = 0; i < richloc->get_num_fixit_hints (); i++)
    {
      const fixit_hint *hint = richloc->get_fixit_hint (i);
      if (!apply_fixit (hint))
	m_valid = false;
    }
}

/* Get the content of the given file, with fix-its applied.
   If any errors occurred in this edit_context, return NULL.
   The ptr should be freed by the caller.  */

char *
edit_context::get_content (const char *filename)
{
  if (!m_valid)
    return NULL;
  edited_file &file = get_or_insert_file (filename);
  return file.get_content ();
}

/* Map a location before the edits to a column number after the edits.
   This method is for the selftests.  */

int
edit_context::get_effective_column (const char *filename, int line,
				    int column)
{
  edited_file *file = get_file (filename);
  if (!file)
    return column;
  return file->get_effective_column (line, column);
}

/* Generate a unified diff.  The resulting string should be freed by the
   caller.  Primarily for selftests.
   If any errors occurred in this edit_context, return NULL.  */

char *
edit_context::generate_diff (bool show_filenames)
{
  if (!m_valid)
    return NULL;

  pretty_printer pp;
  print_diff (&pp, show_filenames);
  return xstrdup (pp_formatted_text (&pp));
}

/* Print a unified diff to PP, showing the changes made within the
   context.  */

void
edit_context::print_diff (pretty_printer *pp, bool show_filenames)
{
  if (!m_valid)
    return;
  diff d (pp, show_filenames);
  m_files.foreach (edited_file::call_print_diff, &d);
}

/* Attempt to apply the given fixit.  Return true if it can be
   applied, or false otherwise.  */

bool
edit_context::apply_fixit (const fixit_hint *hint)
{
  expanded_location start = expand_location (hint->get_start_loc ());
  expanded_location next_loc = expand_location (hint->get_next_loc ());
  if (start.file != next_loc.file)
    return false;
  if (start.line != next_loc.line)
    return false;
  if (start.column == 0)
    return false;
  if (next_loc.column == 0)
    return false;

  edited_file &file = get_or_insert_file (start.file);
  if (!m_valid)
    return false;
  return file.apply_fixit (start.line, start.column, next_loc.column,
			   hint->get_string (),
			   hint->get_length ());
}

/* Locate the edited_file * for FILENAME, if any
   Return NULL if there isn't one.  */

edited_file *
edit_context::get_file (const char *filename)
{
  gcc_assert (filename);
  return m_files.lookup (filename);
}

/* Locate the edited_file for FILENAME, adding one if there isn't one.  */

edited_file &
edit_context::get_or_insert_file (const char *filename)
{
  gcc_assert (filename);

  edited_file *file = get_file (filename);
  if (file)
    return *file;

  /* Not found.  */
  file = new edited_file (filename);
  m_files.insert (filename, file);
  return *file;
}

/* Implementation of class edited_file.  */

/* Callback for m_edited_lines, for comparing line numbers.  */

static int line_comparator (int a, int b)
{
  return a - b;
}

/* edited_file's constructor.  */

edited_file::edited_file (const char *filename)
: m_filename (filename),
  m_edited_lines (line_comparator, NULL, edited_line::delete_cb),
  m_num_lines (-1)
{
}

/* A callback for deleting edited_file *, for use as a
   delete_value_fn for edit_context::m_files.  */

void
edited_file::delete_cb (edited_file *file)
{
  delete file;
}

/* Get the content of the file, with fix-its applied.
   The ptr should be freed by the caller.  */

char *
edited_file::get_content ()
{
  pretty_printer pp;
  if (!print_content (&pp))
    return NULL;
  return xstrdup (pp_formatted_text (&pp));
}

/* Attempt to replace columns START_COLUMN up to but not including NEXT_COLUMN
   of LINE with the string REPLACEMENT_STR of length REPLACEMENT_LEN,
   updating the in-memory copy of the line, and the record of edits to
   the line.  */

bool
edited_file::apply_fixit (int line, int start_column, int next_column,
			  const char *replacement_str,
			  int replacement_len)
{
  edited_line *el = get_or_insert_line (line);
  if (!el)
    return false;
  return el->apply_fixit (start_column, next_column, replacement_str,
			  replacement_len);
}

/* Given line LINE, map from COLUMN in the input file to its current
   column after edits have been applied.  */

int
edited_file::get_effective_column (int line, int column)
{
  const edited_line *el = get_line (line);
  if (!el)
    return column;
  return el->get_effective_column (column);
}

/* Attempt to print the content of the file to PP, with edits applied.
   Return true if successful, false otherwise.  */

bool
edited_file::print_content (pretty_printer *pp)
{
  bool missing_trailing_newline;
  int line_count = get_num_lines (&missing_trailing_newline);
  for (int line_num = 1; line_num <= line_count; line_num++)
    {
      edited_line *el = get_line (line_num);
      if (el)
	el->print_content (pp);
      else
	{
	  char_span line = location_get_source_line (m_filename, line_num);
	  if (!line)
	    return false;
	  for (size_t i = 0; i < line.length (); i++)
	    pp_character (pp, line[i]);
	}
      if (line_num < line_count)
	pp_character (pp, '\n');
    }

  if (!missing_trailing_newline)
    pp_character (pp, '\n');

  return true;
}

/* Print a unified diff to PP, showing any changes that have occurred
   to this file.  */

void
edited_file::print_diff (pretty_printer *pp, bool show_filenames)
{
  if (show_filenames)
    {
      pp_string (pp, colorize_start (pp_show_color (pp), "diff-filename"));
      /* Avoid -Wformat-diag in non-diagnostic output.  */
      pp_string (pp, "--- ");
      pp_string (pp, m_filename);
      pp_newline (pp);
      pp_string (pp, "+++ ");
      pp_string (pp, m_filename);
      pp_newline (pp);
      pp_string (pp, colorize_stop (pp_show_color (pp)));
    }

  edited_line *el = m_edited_lines.min ();

  bool missing_trailing_newline;
  int line_count = get_num_lines (&missing_trailing_newline);

  const int context_lines = 3;

  /* Track new line numbers minus old line numbers.  */

  int line_delta = 0;

  while (el)
    {
      int start_of_hunk = el->get_line_num ();
      start_of_hunk -= context_lines;
      if (start_of_hunk < 1)
	start_of_hunk = 1;

      /* Locate end of hunk, merging in changed lines
	 that are sufficiently close.  */
      while (true)
	{
	  edited_line *next_el
	    = m_edited_lines.successor (el->get_line_num ());
	  if (!next_el)
	    break;

	  int end_of_printed_hunk = el->get_line_num () + context_lines;
	  if (!el->actually_edited_p ())
	    end_of_printed_hunk--;

	  if (end_of_printed_hunk
	      >= next_el->get_line_num () - context_lines)
	    el = next_el;
	  else
	    break;
	}

      int end_of_hunk = el->get_line_num ();
      end_of_hunk += context_lines;
      if (!el->actually_edited_p ())
	end_of_hunk--;
      if (end_of_hunk > line_count)
	end_of_hunk = line_count;

      int new_start_of_hunk = start_of_hunk + line_delta;
      line_delta += print_diff_hunk (pp, start_of_hunk, end_of_hunk,
				     new_start_of_hunk);
      el = m_edited_lines.successor (el->get_line_num ());
    }
}

/* Print one hunk within a unified diff to PP, covering the
   given range of lines.  OLD_START_OF_HUNK and OLD_END_OF_HUNK are
   line numbers in the unedited version of the file.
   NEW_START_OF_HUNK is a line number in the edited version of the file.
   Return the change in the line count within the hunk.  */

int
edited_file::print_diff_hunk (pretty_printer *pp, int old_start_of_hunk,
			      int old_end_of_hunk, int new_start_of_hunk)
{
  int old_num_lines = old_end_of_hunk - old_start_of_hunk + 1;
  int new_num_lines
    = get_effective_line_count (old_start_of_hunk, old_end_of_hunk);

  pp_string (pp, colorize_start (pp_show_color (pp), "diff-hunk"));
  pp_printf (pp, "%s -%i,%i +%i,%i %s",
	     "@@", old_start_of_hunk, old_num_lines,
	     new_start_of_hunk, new_num_lines, "@@\n");
  pp_string (pp, colorize_stop (pp_show_color (pp)));

  int line_num = old_start_of_hunk;
  while (line_num <= old_end_of_hunk)
    {
      edited_line *el = get_line (line_num);
      if (el)
	{
	  /* We have an edited line.
	     Consolidate into runs of changed lines.  */
	  const int first_changed_line_in_run = line_num;
	  while (get_line (line_num))
	    line_num++;
	  const int last_changed_line_in_run = line_num - 1;
	  print_run_of_changed_lines (pp, first_changed_line_in_run,
				      last_changed_line_in_run);
	}
      else
	{
	  /* Unchanged line.  */
	  char_span old_line = location_get_source_line (m_filename, line_num);
	  print_diff_line (pp, ' ', old_line.get_buffer (), old_line.length ());
	  line_num++;
	}
    }

  return new_num_lines - old_num_lines;
}

/* Subroutine of edited_file::print_diff_hunk: given a run of lines
   from START_OF_RUN to END_OF_RUN that all have edited_line instances,
   print the diff to PP.  */

void
edited_file::print_run_of_changed_lines (pretty_printer *pp,
					 int start_of_run,
					 int end_of_run)
{
  /* Show old version of lines.  */
  pp_string (pp, colorize_start (pp_show_color (pp),
				 "diff-delete"));
  for (int line_num = start_of_run;
       line_num <= end_of_run;
       line_num++)
    {
      edited_line *el_in_run = get_line (line_num);
      gcc_assert (el_in_run);
      if (el_in_run->actually_edited_p ())
	{
	  char_span old_line = location_get_source_line (m_filename, line_num);
	  print_diff_line (pp, '-', old_line.get_buffer (),
			   old_line.length ());
	}
    }
  pp_string (pp, colorize_stop (pp_show_color (pp)));

  /* Show new version of lines.  */
  pp_string (pp, colorize_start (pp_show_color (pp),
				 "diff-insert"));
  for (int line_num = start_of_run;
       line_num <= end_of_run;
       line_num++)
    {
      edited_line *el_in_run = get_line (line_num);
      gcc_assert (el_in_run);
      el_in_run->print_diff_lines (pp);
    }
  pp_string (pp, colorize_stop (pp_show_color (pp)));
}

/* Print one line within a diff, starting with PREFIX_CHAR,
   followed by the LINE of content, of length LEN.  LINE is
   not necessarily 0-terminated.  Print a trailing newline.  */

static void
print_diff_line (pretty_printer *pp, char prefix_char,
		 const char *line, int len)
{
  pp_character (pp, prefix_char);
  for (int i = 0; i < len; i++)
    pp_character (pp, line[i]);
  pp_character (pp, '\n');
}

/* Determine the number of lines that will be present after
   editing for the range of lines from OLD_START_OF_HUNK to
   OLD_END_OF_HUNK inclusive.  */

int
edited_file::get_effective_line_count (int old_start_of_hunk,
				       int old_end_of_hunk)
{
  int line_count = 0;
  for (int old_line_num = old_start_of_hunk; old_line_num <= old_end_of_hunk;
       old_line_num++)
    {
      edited_line *el = get_line (old_line_num);
      if (el)
	line_count += el->get_effective_line_count ();
      else
	line_count++;
    }
  return line_count;
}

/* Get the state of LINE within the file, or NULL if it is untouched.  */

edited_line *
edited_file::get_line (int line)
{
  return m_edited_lines.lookup (line);
}

/* Get the state of LINE within the file, creating a state for it
   if necessary.  Return NULL if an error occurs.  */

edited_line *
edited_file::get_or_insert_line (int line)
{
  edited_line *el = get_line (line);
  if (el)
    return el;
  el = new edited_line (m_filename, line);
  if (el->get_content () == NULL)
    {
      delete el;
      return NULL;
    }
  m_edited_lines.insert (line, el);
  return el;
}

/* Get the total number of lines in m_content, writing
   true to *MISSING_TRAILING_NEWLINE if the final line
   if missing a newline, false otherwise.  */

int
edited_file::get_num_lines (bool *missing_trailing_newline)
{
  gcc_assert (missing_trailing_newline);
  if (m_num_lines == -1)
    {
      m_num_lines = 0;
      while (true)
	{
	  char_span line
	    = location_get_source_line (m_filename, m_num_lines + 1);
	  if (line)
	    m_num_lines++;
	  else
	    break;
	}
    }
  *missing_trailing_newline = location_missing_trailing_newline (m_filename);
  return m_num_lines;
}

/* Implementation of class edited_line.  */

/* edited_line's ctor.  */

edited_line::edited_line (const char *filename, int line_num)
: m_line_num (line_num),
  m_content (NULL), m_len (0), m_alloc_sz (0),
  m_line_events (),
  m_predecessors ()
{
  char_span line = location_get_source_line (filename, line_num);
  if (!line)
    return;
  m_len = line.length ();
  ensure_capacity (m_len);
  memcpy (m_content, line.get_buffer (), m_len);
  ensure_terminated ();
}

/* edited_line's dtor.  */

edited_line::~edited_line ()
{
  unsigned i;
  added_line *pred;

  free (m_content);
  FOR_EACH_VEC_ELT (m_predecessors, i, pred)
    delete pred;
}

/* A callback for deleting edited_line *, for use as a
   delete_value_fn for edited_file::m_edited_lines.  */

void
edited_line::delete_cb (edited_line *el)
{
  delete el;
}

/* Map a location before the edits to a column number after the edits,
   within a specific line.  */

int
edited_line::get_effective_column (int orig_column) const
{
  int i;
  line_event *event;
  FOR_EACH_VEC_ELT (m_line_events, i, event)
    orig_column = event->get_effective_column (orig_column);
  return orig_column;
}

/* Attempt to replace columns START_COLUMN up to but not including
   NEXT_COLUMN of the line with the string REPLACEMENT_STR of
   length REPLACEMENT_LEN, updating the in-memory copy of the line,
   and the record of edits to the line.
   Return true if successful; false if an error occurred.  */

bool
edited_line::apply_fixit (int start_column,
			  int next_column,
			  const char *replacement_str,
			  int replacement_len)
{
  /* Handle newlines.  They will only ever be at the end of the
     replacement text, thanks to the filtering in rich_location.  */
  if (replacement_len > 1)
    if (replacement_str[replacement_len - 1] == '\n')
      {
	/* Stash in m_predecessors, stripping off newline.  */
	m_predecessors.safe_push (new added_line (replacement_str,
						  replacement_len - 1));
	return true;
      }

  start_column = get_effective_column (start_column);
  next_column = get_effective_column (next_column);

  int start_offset = start_column - 1;
  int next_offset = next_column - 1;

  gcc_assert (start_offset >= 0);
  gcc_assert (next_offset >= 0);

  if (start_column > next_column)
    return false;
  if (start_offset >= (m_len + 1))
    return false;
  if (next_offset >= (m_len + 1))
    return false;

  size_t victim_len = next_offset - start_offset;

  /* Ensure buffer is big enough.  */
  size_t new_len = m_len + replacement_len - victim_len;
  ensure_capacity (new_len);

  char *suffix = m_content + next_offset;
  gcc_assert (suffix <= m_content + m_len);
  size_t len_suffix = (m_content + m_len) - suffix;

  /* Move successor content into position.  They overlap, so use memmove.  */
  memmove (m_content + start_offset + replacement_len,
	   suffix, len_suffix);

  /* Replace target content.  They don't overlap, so use memcpy.  */
  memcpy (m_content + start_offset,
	  replacement_str,
	  replacement_len);

  m_len = new_len;

  ensure_terminated ();

  /* Record the replacement, so that future changes to the line can have
     their column information adjusted accordingly.  */
  m_line_events.safe_push (line_event (start_column, next_column,
				       replacement_len));
  return true;
}

/* Determine the number of lines that will be present after
   editing for this line.  Typically this is just 1, but
   if newlines have been added before this line, they will
   also be counted.  */

int
edited_line::get_effective_line_count () const
{
  return m_predecessors.length () + 1;
}

/* Subroutine of edited_file::print_content.
   Print this line and any new lines added before it, to PP.  */

void
edited_line::print_content (pretty_printer *pp) const
{
  unsigned i;
  added_line *pred;
  FOR_EACH_VEC_ELT (m_predecessors, i, pred)
    {
      pp_string (pp, pred->get_content ());
      pp_newline (pp);
    }
  pp_string (pp, m_content);
}

/* Subroutine of edited_file::print_run_of_changed_lines for
   printing diff hunks to PP.
   Print the '+' line for this line, and any newlines added
   before it.
   Note that if this edited_line was actually edited, the '-'
   line has already been printed.  If it wasn't, then we merely
   have a placeholder edited_line for adding newlines to, and
   we need to print a ' ' line for the edited_line as we haven't
   printed it yet.  */

void
edited_line::print_diff_lines (pretty_printer *pp) const
{
  unsigned i;
  added_line *pred;
  FOR_EACH_VEC_ELT (m_predecessors, i, pred)
    print_diff_line (pp, '+', pred->get_content (),
		     pred->get_len ());
  if (actually_edited_p ())
    print_diff_line (pp, '+', m_content, m_len);
  else
    print_diff_line (pp, ' ', m_content, m_len);
}

/* Ensure that the buffer for m_content is at least large enough to hold
   a string of length LEN and its 0-terminator, doubling on repeated
   allocations.  */

void
edited_line::ensure_capacity (int len)
{
  /* Allow 1 extra byte for 0-termination.  */
  if (m_alloc_sz < (len + 1))
    {
      size_t new_alloc_sz = (len + 1) * 2;
      m_content = (char *)xrealloc (m_content, new_alloc_sz);
      m_alloc_sz = new_alloc_sz;
    }
}

/* Ensure that m_content is 0-terminated.  */

void
edited_line::ensure_terminated ()
{
  /* 0-terminate the buffer.  */
  gcc_assert (m_len < m_alloc_sz);
  m_content[m_len] = '\0';
}

#if CHECKING_P

/* Selftests of code-editing.  */

namespace selftest {

/* A wrapper class for ensuring that the underlying pointer is freed.  */

template <typename POINTER_T>
class auto_free
{
 public:
  auto_free (POINTER_T p) : m_ptr (p) {}
  ~auto_free () { free (m_ptr); }

  operator POINTER_T () { return m_ptr; }

 private:
  POINTER_T m_ptr;
};

/* Verify that edit_context::get_content works for unedited files.  */

static void
test_get_content ()
{
  /* Test of empty file.  */
  {
    const char *content = ("");
    temp_source_file tmp (SELFTEST_LOCATION, ".c", content);
    edit_context edit;
    auto_free <char *> result = edit.get_content (tmp.get_filename ());
    ASSERT_STREQ ("", result);
  }

  /* Test of simple content.  */
  {
    const char *content = ("/* before */\n"
			   "foo = bar.field;\n"
			   "/* after */\n");
    temp_source_file tmp (SELFTEST_LOCATION, ".c", content);
    edit_context edit;
    auto_free <char *> result = edit.get_content (tmp.get_filename ());
    ASSERT_STREQ ("/* before */\n"
		  "foo = bar.field;\n"
		  "/* after */\n", result);
  }

  /* Test of omitting the trailing newline on the final line.  */
  {
    const char *content = ("/* before */\n"
			   "foo = bar.field;\n"
			   "/* after */");
    temp_source_file tmp (SELFTEST_LOCATION, ".c", content);
    edit_context edit;
    auto_free <char *> result = edit.get_content (tmp.get_filename ());
    /* We should respect the omitted trailing newline.  */
    ASSERT_STREQ ("/* before */\n"
		  "foo = bar.field;\n"
		  "/* after */", result);
  }
}

/* Test applying an "insert" fixit, using insert_before.  */

static void
test_applying_fixits_insert_before (const line_table_case &case_)
{
  /* Create a tempfile and write some text to it.
     .........................0000000001111111.
     .........................1234567890123456.  */
  const char *old_content = ("/* before */\n"
			     "foo = bar.field;\n"
			     "/* after */\n");
  temp_source_file tmp (SELFTEST_LOCATION, ".c", old_content);
  const char *filename = tmp.get_filename ();
  line_table_test ltt (case_);
  linemap_add (line_table, LC_ENTER, false, tmp.get_filename (), 2);

  /* Add a comment in front of "bar.field".  */
  location_t start = linemap_position_for_column (line_table, 7);
  rich_location richloc (line_table, start);
  richloc.add_fixit_insert_before ("/* inserted */");

  if (start > LINE_MAP_MAX_LOCATION_WITH_COLS)
    return;

  edit_context edit;
  edit.add_fixits (&richloc);
  auto_free <char *> new_content = edit.get_content (filename);
  if (start <= LINE_MAP_MAX_LOCATION_WITH_COLS)
    ASSERT_STREQ ("/* before */\n"
		  "foo = /* inserted */bar.field;\n"
		  "/* after */\n", new_content);

  /* Verify that locations on other lines aren't affected by the change.  */
  ASSERT_EQ (100, edit.get_effective_column (filename, 1, 100));
  ASSERT_EQ (100, edit.get_effective_column (filename, 3, 100));

  /* Verify locations on the line before the change.  */
  ASSERT_EQ (1, edit.get_effective_column (filename, 2, 1));
  ASSERT_EQ (6, edit.get_effective_column (filename, 2, 6));

  /* Verify locations on the line at and after the change.  */
  ASSERT_EQ (21, edit.get_effective_column (filename, 2, 7));
  ASSERT_EQ (22, edit.get_effective_column (filename, 2, 8));

  /* Verify diff.  */
  auto_free <char *> diff = edit.generate_diff (false);
  ASSERT_STREQ ("@@ -1,3 +1,3 @@\n"
		" /* before */\n"
		"-foo = bar.field;\n"
		"+foo = /* inserted */bar.field;\n"
		" /* after */\n", diff);
}

/* Test applying an "insert" fixit, using insert_after, with
   a range of length > 1 (to ensure that the end-point of
   the input range is used).  */

static void
test_applying_fixits_insert_after (const line_table_case &case_)
{
  /* Create a tempfile and write some text to it.
     .........................0000000001111111.
     .........................1234567890123456.  */
  const char *old_content = ("/* before */\n"
			     "foo = bar.field;\n"
			     "/* after */\n");
  temp_source_file tmp (SELFTEST_LOCATION, ".c", old_content);
  const char *filename = tmp.get_filename ();
  line_table_test ltt (case_);
  linemap_add (line_table, LC_ENTER, false, tmp.get_filename (), 2);

  /* Add a comment after "field".  */
  location_t start = linemap_position_for_column (line_table, 11);
  location_t finish = linemap_position_for_column (line_table, 15);
  location_t field = make_location (start, start, finish);
  rich_location richloc (line_table, field);
  richloc.add_fixit_insert_after ("/* inserted */");

  if (finish > LINE_MAP_MAX_LOCATION_WITH_COLS)
    return;

  /* Verify that the text was inserted after the end of "field". */
  edit_context edit;
  edit.add_fixits (&richloc);
  auto_free <char *> new_content = edit.get_content (filename);
  ASSERT_STREQ ("/* before */\n"
		"foo = bar.field/* inserted */;\n"
		"/* after */\n", new_content);

  /* Verify diff.  */
  auto_free <char *> diff = edit.generate_diff (false);
  ASSERT_STREQ ("@@ -1,3 +1,3 @@\n"
		" /* before */\n"
		"-foo = bar.field;\n"
		"+foo = bar.field/* inserted */;\n"
		" /* after */\n", diff);
}

/* Test applying an "insert" fixit, using insert_after at the end of
   a line (contrast with test_applying_fixits_insert_after_failure
   below).  */

static void
test_applying_fixits_insert_after_at_line_end (const line_table_case &case_)
{
  /* Create a tempfile and write some text to it.
     .........................0000000001111111.
     .........................1234567890123456.  */
  const char *old_content = ("/* before */\n"
			     "foo = bar.field;\n"
			     "/* after */\n");
  temp_source_file tmp (SELFTEST_LOCATION, ".c", old_content);
  const char *filename = tmp.get_filename ();
  line_table_test ltt (case_);
  linemap_add (line_table, LC_ENTER, false, tmp.get_filename (), 2);

  /* Add a comment after the semicolon.  */
  location_t loc = linemap_position_for_column (line_table, 16);
  rich_location richloc (line_table, loc);
  richloc.add_fixit_insert_after ("/* inserted */");

  if (loc > LINE_MAP_MAX_LOCATION_WITH_COLS)
    return;

  edit_context edit;
  edit.add_fixits (&richloc);
  auto_free <char *> new_content = edit.get_content (filename);
  ASSERT_STREQ ("/* before */\n"
		"foo = bar.field;/* inserted */\n"
		"/* after */\n", new_content);

  /* Verify diff.  */
  auto_free <char *> diff = edit.generate_diff (false);
  ASSERT_STREQ ("@@ -1,3 +1,3 @@\n"
		" /* before */\n"
		"-foo = bar.field;\n"
		"+foo = bar.field;/* inserted */\n"
		" /* after */\n", diff);
}

/* Test of a failed attempt to apply an "insert" fixit, using insert_after,
   due to the relevant linemap ending.  Contrast with
   test_applying_fixits_insert_after_at_line_end above.  */

static void
test_applying_fixits_insert_after_failure (const line_table_case &case_)
{
  /* Create a tempfile and write some text to it.
     .........................0000000001111111.
     .........................1234567890123456.  */
  const char *old_content = ("/* before */\n"
			     "foo = bar.field;\n"
			     "/* after */\n");
  temp_source_file tmp (SELFTEST_LOCATION, ".c", old_content);
  const char *filename = tmp.get_filename ();
  line_table_test ltt (case_);
  linemap_add (line_table, LC_ENTER, false, tmp.get_filename (), 2);

  /* Add a comment after the semicolon.  */
  location_t loc = linemap_position_for_column (line_table, 16);
  rich_location richloc (line_table, loc);

  /* We want a failure of linemap_position_for_loc_and_offset.
     We can do this by starting a new linemap at line 3, so that
     there is no appropriate location value for the insertion point
     within the linemap for line 2.  */
  linemap_add (line_table, LC_ENTER, false, tmp.get_filename (), 3);

  /* The failure fails to happen at the transition point from
     packed ranges to unpacked ranges (where there are some "spare"
     location_t values).  Skip the test there.  */
  if (loc >= LINE_MAP_MAX_LOCATION_WITH_PACKED_RANGES)
    return;

  /* Offsetting "loc" should now fail (by returning the input loc. */
  ASSERT_EQ (loc, linemap_position_for_loc_and_offset (line_table, loc, 1));

  /* Hence attempting to use add_fixit_insert_after at the end of the line
     should now fail.  */
  richloc.add_fixit_insert_after ("/* inserted */");
  ASSERT_TRUE (richloc.seen_impossible_fixit_p ());

  edit_context edit;
  edit.add_fixits (&richloc);
  ASSERT_FALSE (edit.valid_p ());
  ASSERT_EQ (NULL, edit.get_content (filename));
  ASSERT_EQ (NULL, edit.generate_diff (false));
}

/* Test applying an "insert" fixit that adds a newline.  */

static void
test_applying_fixits_insert_containing_newline (const line_table_case &case_)
{
  /* Create a tempfile and write some text to it.
     .........................0000000001111111.
     .........................1234567890123456.  */
  const char *old_content = ("    case 'a':\n" /* line 1. */
			     "      x = a;\n"  /* line 2. */
			     "    case 'b':\n" /* line 3. */
			     "      x = b;\n");/* line 4. */

  temp_source_file tmp (SELFTEST_LOCATION, ".c", old_content);
  const char *filename = tmp.get_filename ();
  line_table_test ltt (case_);
  linemap_add (line_table, LC_ENTER, false, tmp.get_filename (), 3);

  /* Add a "break;" on a line by itself before line 3 i.e. before
     column 1 of line 3. */
  location_t case_start = linemap_position_for_column (line_table, 5);
  location_t case_finish = linemap_position_for_column (line_table, 13);
  location_t case_loc = make_location (case_start, case_start, case_finish);
  rich_location richloc (line_table, case_loc);
  location_t line_start = linemap_position_for_column (line_table, 1);
  richloc.add_fixit_insert_before (line_start, "      break;\n");

  if (case_finish > LINE_MAP_MAX_LOCATION_WITH_COLS)
    return;

  edit_context edit;
  edit.add_fixits (&richloc);
  auto_free <char *> new_content = edit.get_content (filename);
  ASSERT_STREQ (("    case 'a':\n"
		 "      x = a;\n"
		 "      break;\n"
		 "    case 'b':\n"
		 "      x = b;\n"),
		new_content);

  /* Verify diff.  */
  auto_free <char *> diff = edit.generate_diff (false);
  ASSERT_STREQ (("@@ -1,4 +1,5 @@\n"
		 "     case 'a':\n"
		 "       x = a;\n"
		 "+      break;\n"
		 "     case 'b':\n"
		 "       x = b;\n"),
		diff);
}

/* Test applying a "replace" fixit that grows the affected line.  */

static void
test_applying_fixits_growing_replace (const line_table_case &case_)
{
  /* Create a tempfile and write some text to it.
     .........................0000000001111111.
     .........................1234567890123456.  */
  const char *old_content = ("/* before */\n"
			     "foo = bar.field;\n"
			     "/* after */\n");
  temp_source_file tmp (SELFTEST_LOCATION, ".c", old_content);
  const char *filename = tmp.get_filename ();
  line_table_test ltt (case_);
  linemap_add (line_table, LC_ENTER, false, filename, 2);

  /* Replace "field" with "m_field".  */
  location_t start = linemap_position_for_column (line_table, 11);
  location_t finish = linemap_position_for_column (line_table, 15);
  location_t field = make_location (start, start, finish);
  rich_location richloc (line_table, field);
  richloc.add_fixit_replace ("m_field");

  edit_context edit;
  edit.add_fixits (&richloc);
  auto_free <char *> new_content = edit.get_content (filename);
  if (finish <= LINE_MAP_MAX_LOCATION_WITH_COLS)
    {
      ASSERT_STREQ ("/* before */\n"
		    "foo = bar.m_field;\n"
		    "/* after */\n", new_content);

      /* Verify location of ";" after the change.  */
      ASSERT_EQ (18, edit.get_effective_column (filename, 2, 16));

      /* Verify diff.  */
      auto_free <char *> diff = edit.generate_diff (false);
      ASSERT_STREQ ("@@ -1,3 +1,3 @@\n"
		    " /* before */\n"
		    "-foo = bar.field;\n"
		    "+foo = bar.m_field;\n"
		    " /* after */\n", diff);
    }
}

/* Test applying a "replace" fixit that shrinks the affected line.  */

static void
test_applying_fixits_shrinking_replace (const line_table_case &case_)
{
  /* Create a tempfile and write some text to it.
     .........................000000000111111111.
     .........................123456789012345678.  */
  const char *old_content = ("/* before */\n"
			     "foo = bar.m_field;\n"
			     "/* after */\n");
  temp_source_file tmp (SELFTEST_LOCATION, ".c", old_content);
  const char *filename = tmp.get_filename ();
  line_table_test ltt (case_);
  linemap_add (line_table, LC_ENTER, false, filename, 2);

  /* Replace "field" with "m_field".  */
  location_t start = linemap_position_for_column (line_table, 11);
  location_t finish = linemap_position_for_column (line_table, 17);
  location_t m_field = make_location (start, start, finish);
  rich_location richloc (line_table, m_field);
  richloc.add_fixit_replace ("field");

  edit_context edit;
  edit.add_fixits (&richloc);
  auto_free <char *> new_content = edit.get_content (filename);
  if (finish <= LINE_MAP_MAX_LOCATION_WITH_COLS)
    {
      ASSERT_STREQ ("/* before */\n"
		    "foo = bar.field;\n"
		    "/* after */\n", new_content);

      /* Verify location of ";" after the change.  */
      ASSERT_EQ (16, edit.get_effective_column (filename, 2, 18));

      /* Verify diff.  */
      auto_free <char *> diff = edit.generate_diff (false);
      ASSERT_STREQ ("@@ -1,3 +1,3 @@\n"
		    " /* before */\n"
		    "-foo = bar.m_field;\n"
		    "+foo = bar.field;\n"
		    " /* after */\n", diff);
    }
}

/* Replacement fix-it hint containing a newline.  */

static void
test_applying_fixits_replace_containing_newline (const line_table_case &case_)
{
  /* Create a tempfile and write some text to it.
    .........................0000000001111.
    .........................1234567890123.  */
  const char *old_content = "foo = bar ();\n";

  temp_source_file tmp (SELFTEST_LOCATION, ".c", old_content);
  const char *filename = tmp.get_filename ();
  line_table_test ltt (case_);
  linemap_add (line_table, LC_ENTER, false, filename, 1);

  /* Replace the " = " with "\n  = ", as if we were reformatting an
     overly long line.  */
  location_t start = linemap_position_for_column (line_table, 4);
  location_t finish = linemap_position_for_column (line_table, 6);
  location_t loc = linemap_position_for_column (line_table, 13);
  rich_location richloc (line_table, loc);
  source_range range = source_range::from_locations (start, finish);
  richloc.add_fixit_replace (range, "\n  = ");

  /* Newlines are only supported within fix-it hints that
     are at the start of lines (for entirely new lines), hence
     this fix-it should not be displayed.  */
  ASSERT_TRUE (richloc.seen_impossible_fixit_p ());

  if (finish > LINE_MAP_MAX_LOCATION_WITH_COLS)
    return;

  edit_context edit;
  edit.add_fixits (&richloc);
  auto_free <char *> new_content = edit.get_content (filename);
  //ASSERT_STREQ ("foo\n  = bar ();\n", new_content);
}

/* Test applying a "remove" fixit.  */

static void
test_applying_fixits_remove (const line_table_case &case_)
{
  /* Create a tempfile and write some text to it.
     .........................000000000111111111.
     .........................123456789012345678.  */
  const char *old_content = ("/* before */\n"
			     "foo = bar.m_field;\n"
			     "/* after */\n");
  temp_source_file tmp (SELFTEST_LOCATION, ".c", old_content);
  const char *filename = tmp.get_filename ();
  line_table_test ltt (case_);
  linemap_add (line_table, LC_ENTER, false, filename, 2);

  /* Remove ".m_field".  */
  location_t start = linemap_position_for_column (line_table, 10);
  location_t finish = linemap_position_for_column (line_table, 17);
  rich_location richloc (line_table, start);
  source_range range;
  range.m_start = start;
  range.m_finish = finish;
  richloc.add_fixit_remove (range);

  edit_context edit;
  edit.add_fixits (&richloc);
  auto_free <char *> new_content = edit.get_content (filename);
  if (finish <= LINE_MAP_MAX_LOCATION_WITH_COLS)
    {
      ASSERT_STREQ ("/* before */\n"
		    "foo = bar;\n"
		    "/* after */\n", new_content);

      /* Verify location of ";" after the change.  */
      ASSERT_EQ (10, edit.get_effective_column (filename, 2, 18));

      /* Verify diff.  */
      auto_free <char *> diff = edit.generate_diff (false);
      ASSERT_STREQ ("@@ -1,3 +1,3 @@\n"
		    " /* before */\n"
		    "-foo = bar.m_field;\n"
		    "+foo = bar;\n"
		    " /* after */\n", diff);
    }
}

/* Test applying multiple fixits to one line.  */

static void
test_applying_fixits_multiple (const line_table_case &case_)
{
  /* Create a tempfile and write some text to it.
     .........................00000000011111111.
     .........................12345678901234567.  */
  const char *old_content = ("/* before */\n"
			     "foo = bar.field;\n"
			     "/* after */\n");
  temp_source_file tmp (SELFTEST_LOCATION, ".c", old_content);
  const char *filename = tmp.get_filename ();
  line_table_test ltt (case_);
  linemap_add (line_table, LC_ENTER, false, filename, 2);

  location_t c7 = linemap_position_for_column (line_table, 7);
  location_t c9 = linemap_position_for_column (line_table, 9);
  location_t c11 = linemap_position_for_column (line_table, 11);
  location_t c15 = linemap_position_for_column (line_table, 15);
  location_t c17 = linemap_position_for_column (line_table, 17);

  if (c17 > LINE_MAP_MAX_LOCATION_WITH_COLS)
    return;

  /* Add a comment in front of "bar.field".  */
  rich_location insert_a (line_table, c7);
  insert_a.add_fixit_insert_before (c7, "/* alpha */");

  /* Add a comment after "bar.field;".  */
  rich_location insert_b (line_table, c17);
  insert_b.add_fixit_insert_before (c17, "/* beta */");

  /* Replace "bar" with "pub".   */
  rich_location replace_a (line_table, c7);
  replace_a.add_fixit_replace (source_range::from_locations (c7, c9),
			       "pub");

  /* Replace "field" with "meadow".   */
  rich_location replace_b (line_table, c7);
  replace_b.add_fixit_replace (source_range::from_locations (c11, c15),
			       "meadow");

  edit_context edit;
  edit.add_fixits (&insert_a);
  ASSERT_EQ (100, edit.get_effective_column (filename, 1, 100));
  ASSERT_EQ (1, edit.get_effective_column (filename, 2, 1));
  ASSERT_EQ (6, edit.get_effective_column (filename, 2, 6));
  ASSERT_EQ (18, edit.get_effective_column (filename, 2, 7));
  ASSERT_EQ (27, edit.get_effective_column (filename, 2, 16));
  ASSERT_EQ (100, edit.get_effective_column (filename, 3, 100));

  edit.add_fixits (&insert_b);
  edit.add_fixits (&replace_a);
  edit.add_fixits (&replace_b);

  if (c17 <= LINE_MAP_MAX_LOCATION_WITH_COLS)
    {
      auto_free <char *> new_content = edit.get_content (tmp.get_filename ());
      ASSERT_STREQ ("/* before */\n"
		     "foo = /* alpha */pub.meadow;/* beta */\n"
		     "/* after */\n",
		    new_content);

      /* Verify diff.  */
      auto_free <char *> diff = edit.generate_diff (false);
      ASSERT_STREQ ("@@ -1,3 +1,3 @@\n"
		    " /* before */\n"
		    "-foo = bar.field;\n"
		    "+foo = /* alpha */pub.meadow;/* beta */\n"
		    " /* after */\n", diff);
    }
}

/* Subroutine of test_applying_fixits_multiple_lines.
   Add the text "CHANGED: " to the front of the given line.  */

static location_t
change_line (edit_context &edit, int line_num)
{
  const line_map_ordinary *ord_map
    = LINEMAPS_LAST_ORDINARY_MAP (line_table);
  const int column = 1;
  location_t loc =
    linemap_position_for_line_and_column (line_table, ord_map,
					  line_num, column);

  expanded_location exploc = expand_location (loc);
  if (loc <= LINE_MAP_MAX_LOCATION_WITH_COLS)
    {
      ASSERT_EQ (line_num, exploc.line);
      ASSERT_EQ (column, exploc.column);
    }

  rich_location insert (line_table, loc);
  insert.add_fixit_insert_before ("CHANGED: ");
  edit.add_fixits (&insert);
  return loc;
}

/* Subroutine of test_applying_fixits_multiple_lines.
   Add the text "INSERTED\n" in front of the given line.  */

static location_t
insert_line (edit_context &edit, int line_num)
{
  const line_map_ordinary *ord_map
    = LINEMAPS_LAST_ORDINARY_MAP (line_table);
  const int column = 1;
  location_t loc =
    linemap_position_for_line_and_column (line_table, ord_map,
					  line_num, column);

  expanded_location exploc = expand_location (loc);
  if (loc <= LINE_MAP_MAX_LOCATION_WITH_COLS)
    {
      ASSERT_EQ (line_num, exploc.line);
      ASSERT_EQ (column, exploc.column);
    }

  rich_location insert (line_table, loc);
  insert.add_fixit_insert_before ("INSERTED\n");
  edit.add_fixits (&insert);
  return loc;
}

/* Test of editing multiple lines within a long file,
   to ensure that diffs are generated as expected.  */

static void
test_applying_fixits_multiple_lines (const line_table_case &case_)
{
  /* Create a tempfile and write many lines of text to it.  */
  named_temp_file tmp (".txt");
  const char *filename = tmp.get_filename ();
  FILE *f = fopen (filename, "w");
  ASSERT_NE (f, NULL);
  for (int i = 1; i <= 1000; i++)
    fprintf (f, "line %i\n", i);
  fclose (f);

  line_table_test ltt (case_);
  linemap_add (line_table, LC_ENTER, false, filename, 1);
  linemap_position_for_column (line_table, 127);

  edit_context edit;

  /* A run of consecutive lines.  */
  change_line (edit, 2);
  change_line (edit, 3);
  change_line (edit, 4);
  insert_line (edit, 5);

  /* A run of nearby lines, within the contextual limit.  */
  change_line (edit, 150);
  change_line (edit, 151);
  location_t last_loc = change_line (edit, 153);

  if (last_loc > LINE_MAP_MAX_LOCATION_WITH_COLS)
    return;

  /* Verify diff.  */
  auto_free <char *> diff = edit.generate_diff (false);
  ASSERT_STREQ ("@@ -1,7 +1,8 @@\n"
		" line 1\n"
		"-line 2\n"
		"-line 3\n"
		"-line 4\n"
		"+CHANGED: line 2\n"
		"+CHANGED: line 3\n"
		"+CHANGED: line 4\n"
		"+INSERTED\n"
		" line 5\n"
		" line 6\n"
		" line 7\n"
		"@@ -147,10 +148,10 @@\n"
		" line 147\n"
		" line 148\n"
		" line 149\n"
		"-line 150\n"
		"-line 151\n"
		"+CHANGED: line 150\n"
		"+CHANGED: line 151\n"
		" line 152\n"
		"-line 153\n"
		"+CHANGED: line 153\n"
		" line 154\n"
		" line 155\n"
		" line 156\n", diff);

  /* Ensure tmp stays alive until this point, so that the tempfile
     persists until after the generate_diff call.  */
  tmp.get_filename ();
}

/* Test of converting an initializer for a named field from
   the old GCC extension to C99 syntax.
   Exercises a shrinking replacement followed by a growing
   replacement on the same line.  */

static void
test_applying_fixits_modernize_named_init (const line_table_case &case_)
{
  /* Create a tempfile and write some text to it.
     .........................00000000011111111.
     .........................12345678901234567.  */
  const char *old_content = ("/* before */\n"
			     "bar    : 1,\n"
			     "/* after */\n");
  temp_source_file tmp (SELFTEST_LOCATION, ".c", old_content);
  const char *filename = tmp.get_filename ();
  line_table_test ltt (case_);
  linemap_add (line_table, LC_ENTER, false, filename, 2);

  location_t c1 = linemap_position_for_column (line_table, 1);
  location_t c3 = linemap_position_for_column (line_table, 3);
  location_t c8 = linemap_position_for_column (line_table, 8);

  if (c8 > LINE_MAP_MAX_LOCATION_WITH_COLS)
    return;

  /* Replace "bar" with ".".  */
  rich_location r1 (line_table, c8);
  r1.add_fixit_replace (source_range::from_locations (c1, c3),
			".");

  /* Replace ":" with "bar =".   */
  rich_location r2 (line_table, c8);
  r2.add_fixit_replace (source_range::from_locations (c8, c8),
			"bar =");

  /* The order should not matter.  Do r1 then r2. */
  {
    edit_context edit;
    edit.add_fixits (&r1);

    /* Verify state after first replacement.  */
    {
      auto_free <char *> new_content = edit.get_content (tmp.get_filename ());
      /* We should now have:
	 ............00000000011.
	 ............12345678901.  */
      ASSERT_STREQ ("/* before */\n"
		    ".    : 1,\n"
		    "/* after */\n",
		    new_content);
      /* Location of the "1".  */
      ASSERT_EQ (6, edit.get_effective_column (filename, 2, 8));
      /* Location of the ",".  */
      ASSERT_EQ (9, edit.get_effective_column (filename, 2, 11));
    }

    edit.add_fixits (&r2);

    auto_free <char *> new_content = edit.get_content (tmp.get_filename ());
    /* Verify state after second replacement.
       ............00000000011111111.
       ............12345678901234567.  */
    ASSERT_STREQ ("/* before */\n"
		  ".    bar = 1,\n"
		  "/* after */\n",
		  new_content);
  }

  /* Try again, doing r2 then r1; the new_content should be the same.  */
  {
    edit_context edit;
    edit.add_fixits (&r2);
    edit.add_fixits (&r1);
    auto_free <char *> new_content = edit.get_content (tmp.get_filename ());
    /*.............00000000011111111.
      .............12345678901234567.  */
    ASSERT_STREQ ("/* before */\n"
		  ".    bar = 1,\n"
		  "/* after */\n",
		  new_content);
  }
}

/* Test of a fixit affecting a file that can't be read.  */

static void
test_applying_fixits_unreadable_file ()
{
  const char *filename = "this-does-not-exist.txt";
  line_table_test ltt;
  linemap_add (line_table, LC_ENTER, false, filename, 1);

  location_t loc = linemap_position_for_column (line_table, 1);

  rich_location insert (line_table, loc);
  insert.add_fixit_insert_before ("change 1");
  insert.add_fixit_insert_before ("change 2");

  edit_context edit;
  /* Attempting to add the fixits affecting the unreadable file
     should transition the edit from valid to invalid.  */
  ASSERT_TRUE (edit.valid_p ());
  edit.add_fixits (&insert);
  ASSERT_FALSE (edit.valid_p ());
  ASSERT_EQ (NULL, edit.get_content (filename));
  ASSERT_EQ (NULL, edit.generate_diff (false));
}

/* Verify that we gracefully handle an attempt to edit a line
   that's beyond the end of the file.  */

static void
test_applying_fixits_line_out_of_range ()
{
  /* Create a tempfile and write some text to it.
     ........................00000000011111111.
     ........................12345678901234567.  */
  const char *old_content = "One-liner file\n";
  temp_source_file tmp (SELFTEST_LOCATION, ".txt", old_content);
  const char *filename = tmp.get_filename ();
  line_table_test ltt;
  linemap_add (line_table, LC_ENTER, false, filename, 2);

  /* Try to insert a string in line 2.  */
  location_t loc = linemap_position_for_column (line_table, 1);

  rich_location insert (line_table, loc);
  insert.add_fixit_insert_before ("change");

  /* Verify that attempting the insertion puts an edit_context
     into an invalid state.  */
  edit_context edit;
  ASSERT_TRUE (edit.valid_p ());
  edit.add_fixits (&insert);
  ASSERT_FALSE (edit.valid_p ());
  ASSERT_EQ (NULL, edit.get_content (filename));
  ASSERT_EQ (NULL, edit.generate_diff (false));
}

/* Verify the boundary conditions of column values in fix-it
   hints applied to edit_context instances.  */

static void
test_applying_fixits_column_validation (const line_table_case &case_)
{
  /* Create a tempfile and write some text to it.
     ........................00000000011111111.
     ........................12345678901234567.  */
  const char *old_content = "One-liner file\n";
  temp_source_file tmp (SELFTEST_LOCATION, ".txt", old_content);
  const char *filename = tmp.get_filename ();
  line_table_test ltt (case_);
  linemap_add (line_table, LC_ENTER, false, filename, 1);

  location_t c11 = linemap_position_for_column (line_table, 11);
  location_t c14 = linemap_position_for_column (line_table, 14);
  location_t c15 = linemap_position_for_column (line_table, 15);
  location_t c16 = linemap_position_for_column (line_table, 16);

  /* Verify limits of valid columns in insertion fixits.  */

  /* Verify inserting at the end of the line.  */
  {
    rich_location richloc (line_table, c11);
    richloc.add_fixit_insert_before (c15, " change");

    /* Col 15 is at the end of the line, so the insertion
       should succeed.  */
    edit_context edit;
    edit.add_fixits (&richloc);
    auto_free <char *> new_content = edit.get_content (tmp.get_filename ());
    if (c15 <= LINE_MAP_MAX_LOCATION_WITH_COLS)
      ASSERT_STREQ ("One-liner file change\n", new_content);
    else
      ASSERT_EQ (NULL, new_content);
  }

  /* Verify inserting beyond the end of the line.  */
  {
    rich_location richloc (line_table, c11);
    richloc.add_fixit_insert_before (c16, " change");

    /* Col 16 is beyond the end of the line, so the insertion
       should fail gracefully.  */
    edit_context edit;
    ASSERT_TRUE (edit.valid_p ());
    edit.add_fixits (&richloc);
    ASSERT_FALSE (edit.valid_p ());
    ASSERT_EQ (NULL, edit.get_content (filename));
    ASSERT_EQ (NULL, edit.generate_diff (false));
  }

  /* Verify limits of valid columns in replacement fixits.  */

  /* Verify replacing the end of the line.  */
  {
    rich_location richloc (line_table, c11);
    source_range range = source_range::from_locations (c11, c14);
    richloc.add_fixit_replace (range, "change");

    /* Col 14 is at the end of the line, so the replacement
       should succeed.  */
    edit_context edit;
    edit.add_fixits (&richloc);
    auto_free <char *> new_content = edit.get_content (tmp.get_filename ());
    if (c14 <= LINE_MAP_MAX_LOCATION_WITH_COLS)
      ASSERT_STREQ ("One-liner change\n", new_content);
    else
      ASSERT_EQ (NULL, new_content);
  }

  /* Verify going beyond the end of the line.  */
  {
    rich_location richloc (line_table, c11);
    source_range range = source_range::from_locations (c11, c15);
    richloc.add_fixit_replace (range, "change");

    /* Col 15 is after the end of the line, so the replacement
       should fail; verify that the attempt fails gracefully.  */
    edit_context edit;
    ASSERT_TRUE (edit.valid_p ());
    edit.add_fixits (&richloc);
    ASSERT_FALSE (edit.valid_p ());
    ASSERT_EQ (NULL, edit.get_content (filename));
    ASSERT_EQ (NULL, edit.generate_diff (false));
  }
}

/* Run all of the selftests within this file.  */

void
edit_context_cc_tests ()
{
  test_get_content ();
  for_each_line_table_case (test_applying_fixits_insert_before);
  for_each_line_table_case (test_applying_fixits_insert_after);
  for_each_line_table_case (test_applying_fixits_insert_after_at_line_end);
  for_each_line_table_case (test_applying_fixits_insert_after_failure);
  for_each_line_table_case (test_applying_fixits_insert_containing_newline);
  for_each_line_table_case (test_applying_fixits_growing_replace);
  for_each_line_table_case (test_applying_fixits_shrinking_replace);
  for_each_line_table_case (test_applying_fixits_replace_containing_newline);
  for_each_line_table_case (test_applying_fixits_remove);
  for_each_line_table_case (test_applying_fixits_multiple);
  for_each_line_table_case (test_applying_fixits_multiple_lines);
  for_each_line_table_case (test_applying_fixits_modernize_named_init);
  test_applying_fixits_unreadable_file ();
  test_applying_fixits_line_out_of_range ();
  for_each_line_table_case (test_applying_fixits_column_validation);
}

} // namespace selftest

#endif /* CHECKING_P */
