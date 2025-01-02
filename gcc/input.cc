/* Data and functions related to line maps and input files.
   Copyright (C) 2004-2025 Free Software Foundation, Inc.

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
#include "intl.h"
#include "diagnostic.h"
#include "selftest.h"
#include "cpplib.h"

#ifndef HAVE_ICONV
#define HAVE_ICONV 0
#endif

const char *
special_fname_builtin ()
{
  return _("<built-in>");
}

/* Input charset configuration.  */
static const char *default_charset_callback (const char *)
{
  return nullptr;
}

void
file_cache::initialize_input_context (diagnostic_input_charset_callback ccb,
				      bool should_skip_bom)
{
  m_input_context.ccb = (ccb ? ccb : default_charset_callback);
  m_input_context.should_skip_bom = should_skip_bom;
}

/* This is a cache used by get_next_line to store the content of a
   file to be searched for file lines.  */
class file_cache_slot
{
public:
  file_cache_slot ();
  ~file_cache_slot ();

  void dump (FILE *out, int indent) const;
  void DEBUG_FUNCTION dump () const { dump (stderr, 0); }

  bool read_line_num (size_t line_num,
		      char ** line, ssize_t *line_len);

  /* Accessors.  */
  const char *get_file_path () const { return m_file_path; }
  unsigned get_use_count () const { return m_use_count; }
  bool missing_trailing_newline_p () const
  {
    return m_missing_trailing_newline;
  }
  char_span get_full_file_content ();

  void inc_use_count () { m_use_count++; }

  bool create (const file_cache::input_context &in_context,
	       const char *file_path, FILE *fp, unsigned highest_use_count);
  void evict ();
  void set_content (const char *buf, size_t sz);

 private:
  /* These are information used to store a line boundary.  */
  class line_info
  {
  public:
    /* The line number.  It starts from 1.  */
    size_t line_num;

    /* The position (byte count) of the beginning of the line,
       relative to the file data pointer.  This starts at zero.  */
    size_t start_pos;

    /* The position (byte count) of the last byte of the line.  This
       normally points to the '\n' character, or to one byte after the
       last byte of the file, if the file doesn't contain a '\n'
       character.  */
    size_t end_pos;

    line_info (size_t l, size_t s, size_t e)
      : line_num (l), start_pos (s), end_pos (e)
    {}

    line_info ()
      :line_num (0), start_pos (0), end_pos (0)
    {}
  };

  bool needs_read_p () const;
  bool needs_grow_p () const;
  void maybe_grow ();
  bool read_data ();
  bool maybe_read_data ();
  bool get_next_line (char **line, ssize_t *line_len);
  bool read_next_line (char ** line, ssize_t *line_len);
  bool goto_next_line ();

  static const size_t buffer_size = 4 * 1024;
  static const size_t line_record_size = 100;

  /* The number of time this file has been accessed.  This is used
     to designate which file cache to evict from the cache
     array.  */
  unsigned m_use_count;

  /* The file_path is the key for identifying a particular file in
     the cache.
     For libcpp-using code, the underlying buffer for this field is
     owned by the corresponding _cpp_file within the cpp_reader.  */
  const char *m_file_path;

  FILE *m_fp;

  /* This points to the content of the file that we've read so
     far.  */
  char *m_data;

  /* The allocated buffer to be freed may start a little earlier than DATA,
     e.g. if a UTF8 BOM was skipped at the beginning.  */
  int m_alloc_offset;

  /*  The size of the DATA array above.*/
  size_t m_size;

  /* The number of bytes read from the underlying file so far.  This
     must be less (or equal) than SIZE above.  */
  size_t m_nb_read;

  /* The index of the beginning of the current line.  */
  size_t m_line_start_idx;

  /* The number of the previous line read.  This starts at 1.  Zero
     means we've read no line so far.  */
  size_t m_line_num;

  /* This is the total number of lines of the current file.  At the
     moment, we try to get this information from the line map
     subsystem.  Note that this is just a hint.  When using the C++
     front-end, this hint is correct because the input file is then
     completely tokenized before parsing starts; so the line map knows
     the number of lines before compilation really starts.  For e.g,
     the C front-end, it can happen that we start emitting diagnostics
     before the line map has seen the end of the file.  */
  size_t m_total_lines;

  /* Could this file be missing a trailing newline on its final line?
     Initially true (to cope with empty files), set to true/false
     as each line is read.  */
  bool m_missing_trailing_newline;

  /* This is a record of the beginning and end of the lines we've seen
     while reading the file.  This is useful to avoid walking the data
     from the beginning when we are asked to read a line that is
     before LINE_START_IDX above.  Note that the maximum size of this
     record is line_record_size, so that the memory consumption
     doesn't explode.  We thus scale total_lines down to
     line_record_size.  */
  vec<line_info, va_heap> m_line_record;

  void offset_buffer (int offset)
  {
    gcc_assert (offset < 0 ? m_alloc_offset + offset >= 0
		: (size_t) offset <= m_size);
    gcc_assert (m_data);
    m_alloc_offset += offset;
    m_data += offset;
    m_size -= offset;
  }

};

static const char *
find_end_of_line (const char *s, size_t len);

/* Current position in real source file.  */

location_t input_location = UNKNOWN_LOCATION;

class line_maps *line_table;

/* A stashed copy of "line_table" for use by selftest::line_table_test.
   This needs to be a global so that it can be a GC root, and thus
   prevent the stashed copy from being garbage-collected if the GC runs
   during a line_table_test.  */

class line_maps *saved_line_table;

/* Expand the source location LOC into a human readable location.  If
   LOC resolves to a builtin location, the file name of the readable
   location is set to the string "<built-in>". If EXPANSION_POINT_P is
   TRUE and LOC is virtual, then it is resolved to the expansion
   point of the involved macro.  Otherwise, it is resolved to the
   spelling location of the token.

   When resolving to the spelling location of the token, if the
   resulting location is for a built-in location (that is, it has no
   associated line/column) in the context of a macro expansion, the
   returned location is the first one (while unwinding the macro
   location towards its expansion point) that is in real source
   code.

   ASPECT controls which part of the location to use.  */

static expanded_location
expand_location_1 (const line_maps *set,
		   location_t loc,
		   bool expansion_point_p,
		   enum location_aspect aspect)
{
  expanded_location xloc;
  const line_map_ordinary *map;
  enum location_resolution_kind lrk = LRK_MACRO_EXPANSION_POINT;
  tree block = NULL;

  if (IS_ADHOC_LOC (loc))
    {
      block = LOCATION_BLOCK (loc);
      loc = LOCATION_LOCUS (loc);
    }

  memset (&xloc, 0, sizeof (xloc));

  if (loc >= RESERVED_LOCATION_COUNT)
    {
      if (!expansion_point_p)
	{
	  /* We want to resolve LOC to its spelling location.

	     But if that spelling location is a reserved location that
	     appears in the context of a macro expansion (like for a
	     location for a built-in token), let's consider the first
	     location (toward the expansion point) that is not reserved;
	     that is, the first location that is in real source code.  */
	  loc = linemap_unwind_to_first_non_reserved_loc (set,
							  loc, NULL);
	  lrk = LRK_SPELLING_LOCATION;
	}
      loc = linemap_resolve_location (set, loc, lrk, &map);

      /* loc is now either in an ordinary map, or is a reserved location.
	 If it is a compound location, the caret is in a spelling location,
	 but the start/finish might still be a virtual location.
	 Depending of what the caller asked for, we may need to recurse
	 one level in order to resolve any virtual locations in the
	 end-points.  */
      switch (aspect)
	{
	default:
	  gcc_unreachable ();
	  /* Fall through.  */
	case LOCATION_ASPECT_CARET:
	  break;
	case LOCATION_ASPECT_START:
	  {
	    location_t start = get_start (loc);
	    if (start != loc)
	      return expand_location_1 (set, start, expansion_point_p, aspect);
	  }
	  break;
	case LOCATION_ASPECT_FINISH:
	  {
	    location_t finish = get_finish (loc);
	    if (finish != loc)
	      return expand_location_1 (set, finish, expansion_point_p, aspect);
	  }
	  break;
	}
      xloc = linemap_expand_location (set, map, loc);
    }

  xloc.data = block;
  if (loc <= BUILTINS_LOCATION)
    xloc.file = loc == UNKNOWN_LOCATION ? NULL : special_fname_builtin ();

  return xloc;
}

/* Return the total lines number that have been read so far by the
   line map (in the preprocessor) so far.  For languages like C++ that
   entirely preprocess the input file before starting to parse, this
   equals the actual number of lines of the file.  */

static size_t
total_lines_num (const char *file_path)
{
  size_t r = 0;
  location_t l = 0;
  if (linemap_get_file_highest_location (line_table, file_path, &l))
    {
      gcc_assert (l >= RESERVED_LOCATION_COUNT);
      expanded_location xloc = expand_location (l);
      r = xloc.line;
    }
  return r;
}

/* Lookup the cache used for the content of a given file accessed by
   caret diagnostic.  Return the found cached file, or NULL if no
   cached file was found.  */

file_cache_slot *
file_cache::lookup_file (const char *file_path)
{
  gcc_assert (file_path);

  /* This will contain the found cached file.  */
  file_cache_slot *r = NULL;
  for (unsigned i = 0; i < num_file_slots; ++i)
    {
      file_cache_slot *c = &m_file_slots[i];
      if (c->get_file_path () && !strcmp (c->get_file_path (), file_path))
	{
	  c->inc_use_count ();
	  r = c;
	}
    }

  if (r)
    r->inc_use_count ();

  return r;
}

/* Purge any mention of FILENAME from the cache of files used for
   printing source code.  For use in selftests when working
   with tempfiles.  */

void
file_cache::forcibly_evict_file (const char *file_path)
{
  gcc_assert (file_path);

  file_cache_slot *r = lookup_file (file_path);
  if (!r)
    /* Not found.  */
    return;

  r->evict ();
}

/* Determine if FILE_PATH missing a trailing newline on its final line.
   Only valid to call once all of the file has been loaded, by
   requesting a line number beyond the end of the file.  */

bool
file_cache::missing_trailing_newline_p (const char *file_path)
{
  gcc_assert (file_path);

  file_cache_slot *r = lookup_or_add_file (file_path);
  return r->missing_trailing_newline_p ();
}

void
file_cache::add_buffered_content (const char *file_path,
				  const char *buffer,
				  size_t sz)
{
  gcc_assert (file_path);

  file_cache_slot *r = lookup_file (file_path);
  if (!r)
    {
      unsigned highest_use_count = 0;
      r = evicted_cache_tab_entry (&highest_use_count);
      if (!r->create (m_input_context, file_path, nullptr, highest_use_count))
	return;
    }

  r->set_content (buffer, sz);
}

void
file_cache_slot::evict ()
{
  m_file_path = NULL;
  if (m_fp)
    fclose (m_fp);
  m_fp = NULL;
  m_nb_read = 0;
  m_line_start_idx = 0;
  m_line_num = 0;
  m_line_record.truncate (0);
  m_use_count = 0;
  m_total_lines = 0;
  m_missing_trailing_newline = true;
}

/* Return the file cache that has been less used, recently, or the
   first empty one.  If HIGHEST_USE_COUNT is non-null,
   *HIGHEST_USE_COUNT is set to the highest use count of the entries
   in the cache table.  */

file_cache_slot*
file_cache::evicted_cache_tab_entry (unsigned *highest_use_count)
{
  file_cache_slot *to_evict = &m_file_slots[0];
  unsigned huc = to_evict->get_use_count ();
  for (unsigned i = 1; i < num_file_slots; ++i)
    {
      file_cache_slot *c = &m_file_slots[i];
      bool c_is_empty = (c->get_file_path () == NULL);

      if (c->get_use_count () < to_evict->get_use_count ()
	  || (to_evict->get_file_path () && c_is_empty))
	/* We evict C because it's either an entry with a lower use
	   count or one that is empty.  */
	to_evict = c;

      if (huc < c->get_use_count ())
	huc = c->get_use_count ();

      if (c_is_empty)
	/* We've reached the end of the cache; subsequent elements are
	   all empty.  */
	break;
    }

  if (highest_use_count)
    *highest_use_count = huc;

  return to_evict;
}

/* Create the cache used for the content of a given file to be
   accessed by caret diagnostic.  This cache is added to an array of
   cache and can be retrieved by lookup_file_in_cache_tab.  This
   function returns the created cache.  Note that only the last
   num_file_slots files are cached.

   This can return nullptr if the FILE_PATH can't be opened for
   reading, or if the content can't be converted to the input_charset.  */

file_cache_slot*
file_cache::add_file (const char *file_path)
{

  FILE *fp = fopen (file_path, "r");
  if (fp == NULL)
    return NULL;

  unsigned highest_use_count = 0;
  file_cache_slot *r = evicted_cache_tab_entry (&highest_use_count);
  if (!r->create (m_input_context, file_path, fp, highest_use_count))
    return NULL;
  return r;
}

/* Get a borrowed char_span to the full content of this file
   as decoded according to the input charset, encoded as UTF-8.  */

char_span
file_cache_slot::get_full_file_content ()
{
  char *line;
  ssize_t line_len;
  while (get_next_line (&line, &line_len))
    {
    }
  return char_span (m_data, m_nb_read);
}

/* Populate this slot for use on FILE_PATH and FP, dropping any
   existing cached content within it.  */

bool
file_cache_slot::create (const file_cache::input_context &in_context,
			 const char *file_path, FILE *fp,
			 unsigned highest_use_count)
{
  m_file_path = file_path;
  if (m_fp)
    fclose (m_fp);
  m_fp = fp;
  if (m_alloc_offset)
    offset_buffer (-m_alloc_offset);
  m_nb_read = 0;
  m_line_start_idx = 0;
  m_line_num = 0;
  m_line_record.truncate (0);
  /* Ensure that this cache entry doesn't get evicted next time
     add_file_to_cache_tab is called.  */
  m_use_count = ++highest_use_count;
  m_total_lines = total_lines_num (file_path);
  m_missing_trailing_newline = true;


  /* Check the input configuration to determine if we need to do any
     transformations, such as charset conversion or BOM skipping.  */
  if (const char *input_charset = in_context.ccb (file_path))
    {
      /* Need a full-blown conversion of the input charset.  */
      fclose (m_fp);
      m_fp = NULL;
      const cpp_converted_source cs
	= cpp_get_converted_source (file_path, input_charset);
      if (!cs.data)
	return false;
      if (m_data)
	XDELETEVEC (m_data);
      m_data = cs.data;
      m_nb_read = m_size = cs.len;
      m_alloc_offset = cs.data - cs.to_free;
    }
  else if (in_context.should_skip_bom)
    {
      if (read_data ())
	{
	  const int offset = cpp_check_utf8_bom (m_data, m_nb_read);
	  offset_buffer (offset);
	  m_nb_read -= offset;
	}
    }

  return true;
}

void
file_cache_slot::set_content (const char *buf, size_t sz)
{
  m_data = (char *)xmalloc (sz);
  memcpy (m_data, buf, sz);
  m_nb_read = m_size = sz;
  m_alloc_offset = 0;

  if (m_fp)
    {
      fclose (m_fp);
      m_fp = nullptr;
    }

  /* Compute m_total_lines based on content of buffer.  */
  m_total_lines = 0;
  const char *line_start = m_data;
  size_t remaining_size = sz;
  while (const char *line_end = find_end_of_line (line_start, remaining_size))
    {
      ++m_total_lines;
      remaining_size -= line_end + 1 - line_start;
      line_start = line_end + 1;
    }
}

/* file_cache's ctor.  */

file_cache::file_cache ()
: m_file_slots (new file_cache_slot[num_file_slots])
{
  initialize_input_context (nullptr, false);
}

/* file_cache's dtor.  */

file_cache::~file_cache ()
{
  delete[] m_file_slots;
}

void
file_cache::dump (FILE *out, int indent) const
{
  for (size_t i = 0; i < num_file_slots; ++i)
    {
      fprintf (out, "%*sslot[%i]:\n", indent, "", (int)i);
      m_file_slots[i].dump (out, indent + 2);
    }
}

void
file_cache::dump () const
{
  dump (stderr, 0);
}

/* Lookup the cache used for the content of a given file accessed by
   caret diagnostic.  If no cached file was found, create a new cache
   for this file, add it to the array of cached file and return
   it.

   This can return nullptr on a cache miss if FILE_PATH can't be opened for
   reading, or if the content can't be converted to the input_charset.  */

file_cache_slot*
file_cache::lookup_or_add_file (const char *file_path)
{
  file_cache_slot *r = lookup_file (file_path);
  if (r == NULL)
    r = add_file (file_path);
  return r;
}

/* Default constructor for a cache of file used by caret
   diagnostic.  */

file_cache_slot::file_cache_slot ()
: m_use_count (0), m_file_path (NULL), m_fp (NULL), m_data (0),
  m_alloc_offset (0), m_size (0), m_nb_read (0), m_line_start_idx (0),
  m_line_num (0), m_total_lines (0), m_missing_trailing_newline (true)
{
  m_line_record.create (0);
}

/* Destructor for a cache of file used by caret diagnostic.  */

file_cache_slot::~file_cache_slot ()
{
  if (m_fp)
    {
      fclose (m_fp);
      m_fp = NULL;
    }
  if (m_data)
    {
      offset_buffer (-m_alloc_offset);
      XDELETEVEC (m_data);
      m_data = 0;
    }
  m_line_record.release ();
}

void
file_cache_slot::dump (FILE *out, int indent) const
{
  if (!m_file_path)
    {
      fprintf (out, "%*s(unused)\n", indent, "");
      return;
    }
  fprintf (out, "%*sfile_path: %s\n", indent, "", m_file_path);
  fprintf (out, "%*sfp: %p\n", indent, "", (void *)m_fp);
  fprintf (out, "%*sneeds_read_p: %i\n", indent, "", (int)needs_read_p ());
  fprintf (out, "%*sneeds_grow_p: %i\n", indent, "", (int)needs_grow_p ());
  fprintf (out, "%*suse_count: %i\n", indent, "", m_use_count);
  fprintf (out, "%*ssize: %zi\n", indent, "", m_size);
  fprintf (out, "%*snb_read: %zi\n", indent, "", m_nb_read);
  fprintf (out, "%*sstart_line_idx: %zi\n", indent, "", m_line_start_idx);
  fprintf (out, "%*sline_num: %zi\n", indent, "", m_line_num);
  fprintf (out, "%*stotal_lines: %zi\n", indent, "", m_total_lines);
  fprintf (out, "%*smissing_trailing_newline: %i\n",
	   indent, "", (int)m_missing_trailing_newline);
  fprintf (out, "%*sline records (%i):\n",
	   indent, "", m_line_record.length ());
  for (auto &line : m_line_record)
    fprintf (out, "%*sline %zi: byte offsets: %zi-%zi\n",
	     indent + 2, "",
	     line.line_num, line.start_pos, line.end_pos);
}

/* Returns TRUE iff the cache would need to be filled with data coming
   from the file.  That is, either the cache is empty or full or the
   current line is empty.  Note that if the cache is full, it would
   need to be extended and filled again.  */

bool
file_cache_slot::needs_read_p () const
{
  return m_fp && (m_nb_read == 0
	  || m_nb_read == m_size
	  || (m_line_start_idx >= m_nb_read - 1));
}

/*  Return TRUE iff the cache is full and thus needs to be
    extended.  */

bool
file_cache_slot::needs_grow_p () const
{
  return m_nb_read == m_size;
}

/* Grow the cache if it needs to be extended.  */

void
file_cache_slot::maybe_grow ()
{
  if (!needs_grow_p ())
    return;

  if (!m_data)
    {
      gcc_assert (m_size == 0 && m_alloc_offset == 0);
      m_size = buffer_size;
      m_data = XNEWVEC (char, m_size);
    }
  else
    {
      const int offset = m_alloc_offset;
      offset_buffer (-offset);
      m_size *= 2;
      m_data = XRESIZEVEC (char, m_data, m_size);
      offset_buffer (offset);
    }
}

/*  Read more data into the cache.  Extends the cache if need be.
    Returns TRUE iff new data could be read.  */

bool
file_cache_slot::read_data ()
{
  if (feof (m_fp) || ferror (m_fp))
    return false;

  maybe_grow ();

  char * from = m_data + m_nb_read;
  size_t to_read = m_size - m_nb_read;
  size_t nb_read = fread (from, 1, to_read, m_fp);

  if (ferror (m_fp))
    return false;

  m_nb_read += nb_read;
  return !!nb_read;
}

/* Read new data iff the cache needs to be filled with more data
   coming from the file FP.  Return TRUE iff the cache was filled with
   mode data.  */

bool
file_cache_slot::maybe_read_data ()
{
  if (!needs_read_p ())
    return false;
  return read_data ();
}

/* Helper function for file_cache_slot::get_next_line (), to find the end of
   the next line.  Returns with the memchr convention, i.e. nullptr if a line
   terminator was not found.  We need to determine line endings in the same
   manner that libcpp does: any of \n, \r\n, or \r is a line ending.  */

static const char *
find_end_of_line (const char *s, size_t len)
{
  for (const auto end = s + len; s != end; ++s)
    {
      if (*s == '\n')
	return s;
      if (*s == '\r')
	{
	  const auto next = s + 1;
	  if (next == end)
	    {
	      /* Don't find the line ending if \r is the very last character
		 in the buffer; we do not know if it's the end of the file or
		 just the end of what has been read so far, and we wouldn't
		 want to break in the middle of what's actually a \r\n
		 sequence.  Instead, we will handle the case of a file ending
		 in a \r later.  */
	      break;
	    }
	  return (*next == '\n' ? next : s);
	}
    }
  return nullptr;
}

/* Read a new line from file FP, using C as a cache for the data
   coming from the file.  Upon successful completion, *LINE is set to
   the beginning of the line found.  *LINE points directly in the
   line cache and is only valid until the next call of get_next_line.
   *LINE_LEN is set to the length of the line.  Note that the line
   does not contain any terminal delimiter.  This function returns
   true if some data was read or process from the cache, false
   otherwise.  Note that subsequent calls to get_next_line might
   make the content of *LINE invalid.  */

bool
file_cache_slot::get_next_line (char **line, ssize_t *line_len)
{
  /* Fill the cache with data to process.  */
  maybe_read_data ();

  size_t remaining_size = m_nb_read - m_line_start_idx;
  if (remaining_size == 0)
    /* There is no more data to process.  */
    return false;

  const char *line_start = m_data + m_line_start_idx;

  const char *next_line_start = NULL;
  size_t len = 0;
  const char *line_end = find_end_of_line (line_start, remaining_size);
  if (line_end == NULL)
    {
      /* We haven't found an end-of-line delimiter in the cache.
	 Fill the cache with more data from the file and look again.  */
      while (maybe_read_data ())
	{
	  line_start = m_data + m_line_start_idx;
	  remaining_size = m_nb_read - m_line_start_idx;
	  line_end = find_end_of_line (line_start, remaining_size);
	  if (line_end != NULL)
	    {
	      next_line_start = line_end + 1;
	      break;
	    }
	}
      if (line_end == NULL)
	{
	  /* We've loaded all the file into the cache and still no
	     terminator.  Let's say the line ends up at one byte past the
	     end of the file.  This is to stay consistent with the case
	     of when the line ends up with a terminator and line_end points to
	     that.  That consistency is useful below in the len calculation.

	     If the file ends in a \r, we didn't identify it as a line
	     terminator above, so do that now instead.  */
	  line_end = m_data + m_nb_read;
	  if (m_nb_read && line_end[-1] == '\r')
	    {
	      --line_end;
	      m_missing_trailing_newline = false;
	    }
	  else
	    m_missing_trailing_newline = true;
	}
      else
	m_missing_trailing_newline = false;
    }
  else
    {
      next_line_start = line_end + 1;
      m_missing_trailing_newline = false;
    }

  if (m_fp && ferror (m_fp))
    return false;

  /* At this point, we've found the end of the of line.  It either points to
     the line terminator or to one byte after the last byte of the file.  */
  gcc_assert (line_end != NULL);

  len = line_end - line_start;

  if (m_line_start_idx < m_nb_read)
    *line = const_cast<char *> (line_start);

  ++m_line_num;

  /* Before we update our line record, make sure the hint about the
     total number of lines of the file is correct.  If it's not, then
     we give up recording line boundaries from now on.  */
  bool update_line_record = true;
  if (m_line_num > m_total_lines)
    update_line_record = false;

    /* Now update our line record so that re-reading lines from the
     before m_line_start_idx is faster.  */
  if (update_line_record
      && m_line_record.length () < line_record_size)
    {
      /* If the file lines fits in the line record, we just record all
	 its lines ...*/
      if (m_total_lines <= line_record_size
	  && m_line_num > m_line_record.length ())
	m_line_record.safe_push
	  (file_cache_slot::line_info (m_line_num,
				       m_line_start_idx,
				       line_end - m_data));
      else if (m_total_lines > line_record_size)
	{
	  /* ... otherwise, we just scale total_lines down to
	     (line_record_size lines.  */
	  size_t n = (m_line_num * line_record_size) / m_total_lines;
	  if (m_line_record.length () == 0
	      || n >= m_line_record.length ())
	    m_line_record.safe_push
	      (file_cache_slot::line_info (m_line_num,
					   m_line_start_idx,
					   line_end - m_data));
	}
    }

  /* Update m_line_start_idx so that it points to the next line to be
     read.  */
  if (next_line_start)
    m_line_start_idx = next_line_start - m_data;
  else
    /* We didn't find any terminal '\n'.  Let's consider that the end
       of line is the end of the data in the cache.  The next
       invocation of get_next_line will either read more data from the
       underlying file or return false early because we've reached the
       end of the file.  */
    m_line_start_idx = m_nb_read;

  *line_len = len;

  return true;
}

/* Consume the next bytes coming from the cache (or from its
   underlying file if there are remaining unread bytes in the file)
   until we reach the next end-of-line (or end-of-file).  There is no
   copying from the cache involved.  Return TRUE upon successful
   completion.  */

bool
file_cache_slot::goto_next_line ()
{
  char *l;
  ssize_t len;

  return get_next_line (&l, &len);
}

/* Read an arbitrary line number LINE_NUM from the file cached in C.
   If the line was read successfully, *LINE points to the beginning
   of the line in the file cache and *LINE_LEN is the length of the
   line.  *LINE is not nul-terminated, but may contain zero bytes.
   *LINE is only valid until the next call of read_line_num.
   This function returns bool if a line was read.  */

bool
file_cache_slot::read_line_num (size_t line_num,
		       char ** line, ssize_t *line_len)
{
  gcc_assert (line_num > 0);

  if (line_num <= m_line_num)
    {
      /* We've been asked to read lines that are before m_line_num.
	 So lets use our line record (if it's not empty) to try to
	 avoid re-reading the file from the beginning again.  */

      if (m_line_record.is_empty ())
	{
	  m_line_start_idx = 0;
	  m_line_num = 0;
	}
      else
	{
	  file_cache_slot::line_info *i = NULL;
	  if (m_total_lines <= line_record_size)
	    {
	      /* In languages where the input file is not totally
		 preprocessed up front, the m_total_lines hint
		 can be smaller than the number of lines of the
		 file.  In that case, only the first
		 m_total_lines have been recorded.

		 Otherwise, the first m_total_lines we've read have
		 their start/end recorded here.  */
	      i = (line_num <= m_total_lines)
		? &m_line_record[line_num - 1]
		: &m_line_record[m_total_lines - 1];
	      gcc_assert (i->line_num <= line_num);
	    }
	  else
	    {
	      /*  So the file had more lines than our line record
		  size.  Thus the number of lines we've recorded has
		  been scaled down to line_record_size.  Let's
		  pick the start/end of the recorded line that is
		  closest to line_num.  */
	      size_t n = (line_num <= m_total_lines)
		? line_num * line_record_size / m_total_lines
		: m_line_record.length () - 1;
	      if (n < m_line_record.length ())
		{
		  i = &m_line_record[n];
		  gcc_assert (i->line_num <= line_num);
		}
	    }

	  if (i && i->line_num == line_num)
	    {
	      /* We have the start/end of the line.  */
	      *line = m_data + i->start_pos;
	      *line_len = i->end_pos - i->start_pos;
	      return true;
	    }

	  if (i)
	    {
	      m_line_start_idx = i->start_pos;
	      m_line_num = i->line_num - 1;
	    }
	  else
	    {
	      m_line_start_idx = 0;
	      m_line_num = 0;
	    }
	}
    }

  /*  Let's walk from line m_line_num up to line_num - 1, without
      copying any line.  */
  while (m_line_num < line_num - 1)
    if (!goto_next_line ())
      return false;

  /* The line we want is the next one.  Let's read and copy it back to
     the caller.  */
  return get_next_line (line, line_len);
}

/* Return the physical source line that corresponds to FILE_PATH/LINE.
   The line is not nul-terminated.  The returned pointer is only
   valid until the next call of location_get_source_line.
   Note that the line can contain several null characters,
   so the returned value's length has the actual length of the line.
   If the function fails, a NULL char_span is returned.  */

char_span
file_cache::get_source_line (const char *file_path, int line)
{
  char *buffer = NULL;
  ssize_t len;

  if (line == 0)
    return char_span (NULL, 0);

  if (file_path == NULL)
    return char_span (NULL, 0);

  file_cache_slot *c = lookup_or_add_file (file_path);
  if (c == NULL)
    return char_span (NULL, 0);

  bool read = c->read_line_num (line, &buffer, &len);
  if (!read)
    return char_span (NULL, 0);

  return char_span (buffer, len);
}

/* Return a NUL-terminated copy of the source text between two locations, or
   NULL if the arguments are invalid.  The caller is responsible for freeing
   the return value.  */

char *
get_source_text_between (file_cache &fc, location_t start, location_t end)
{
  expanded_location expstart =
    expand_location_to_spelling_point (start, LOCATION_ASPECT_START);
  expanded_location expend =
    expand_location_to_spelling_point (end, LOCATION_ASPECT_FINISH);

  /* If the locations are in different files or the end comes before the
     start, give up and return nothing.  */
  if (!expstart.file || !expend.file)
    return NULL;
  if (strcmp (expstart.file, expend.file) != 0)
    return NULL;
  if (expstart.line > expend.line)
    return NULL;
  if (expstart.line == expend.line
      && expstart.column > expend.column)
    return NULL;
  /* These aren't real column numbers, give up.  */
  if (expstart.column == 0 || expend.column == 0)
    return NULL;

  /* For a single line we need to trim both edges.  */
  if (expstart.line == expend.line)
    {
      char_span line = fc.get_source_line (expstart.file, expstart.line);
      if (line.length () < 1)
	return NULL;
      int s = expstart.column - 1;
      int len = expend.column - s;
      if (line.length () < (size_t)expend.column)
	return NULL;
      return line.subspan (s, len).xstrdup ();
    }

  struct obstack buf_obstack;
  obstack_init (&buf_obstack);

  /* Loop through all lines in the range and append each to buf; may trim
     parts of the start and end lines off depending on column values.  */
  for (int lnum = expstart.line; lnum <= expend.line; ++lnum)
    {
      char_span line = fc.get_source_line (expstart.file, lnum);
      if (line.length () < 1 && (lnum != expstart.line && lnum != expend.line))
	continue;

      /* For the first line in the range, only start at expstart.column */
      if (lnum == expstart.line)
	{
	  unsigned off = expstart.column - 1;
	  if (line.length () < off)
	    return NULL;
	  line = line.subspan (off, line.length() - off);
	}
      /* For the last line, don't go past expend.column */
      else if (lnum == expend.line)
	{
	  if (line.length () < (size_t)expend.column)
	    return NULL;
	  line = line.subspan (0, expend.column);
	}

      /* Combine spaces at the beginning of later lines.  */
      if (lnum > expstart.line)
	{
	  unsigned off;
	  for (off = 0; off < line.length(); ++off)
	    if (line[off] != ' ' && line[off] != '\t')
	      break;
	  if (off > 0)
	    {
	      obstack_1grow (&buf_obstack, ' ');
	      line = line.subspan (off, line.length() - off);
	    }
	}

      /* This does not include any trailing newlines.  */
      obstack_grow (&buf_obstack, line.get_buffer (), line.length ());
    }

  /* NUL-terminate and finish the buf obstack.  */
  obstack_1grow (&buf_obstack, 0);
  const char *buf = (const char *) obstack_finish (&buf_obstack);

  return xstrdup (buf);
}


char_span
file_cache::get_source_file_content (const char *file_path)
{
  file_cache_slot *c = lookup_or_add_file (file_path);
  if (c == nullptr)
    return char_span (nullptr, 0);
  return c->get_full_file_content ();
}

/* Test if the location originates from the spelling location of a
   builtin-tokens.  That is, return TRUE if LOC is a (possibly
   virtual) location of a built-in token that appears in the expansion
   list of a macro.  Please note that this function also works on
   tokens that result from built-in tokens.  For instance, the
   function would return true if passed a token "4" that is the result
   of the expansion of the built-in __LINE__ macro.  */
bool
is_location_from_builtin_token (location_t loc)
{
  const line_map_ordinary *map = NULL;
  loc = linemap_resolve_location (line_table, loc,
				  LRK_SPELLING_LOCATION, &map);
  return loc == BUILTINS_LOCATION;
}

/* Expand the source location LOC into a human readable location.  If
   LOC is virtual, it resolves to the expansion point of the involved
   macro.  If LOC resolves to a builtin location, the file name of the
   readable location is set to the string "<built-in>".  */

expanded_location
expand_location (location_t loc)
{
  return expand_location_1 (line_table, loc, /*expansion_point_p=*/true,
			    LOCATION_ASPECT_CARET);
}

/* Expand the source location LOC into a human readable location.  If
   LOC is virtual, it resolves to the expansion location of the
   relevant macro.  If LOC resolves to a builtin location, the file
   name of the readable location is set to the string
   "<built-in>".  */

expanded_location
expand_location_to_spelling_point (location_t loc,
				   enum location_aspect aspect)
{
  return expand_location_1 (line_table, loc, /*expansion_point_p=*/false,
			    aspect);
}

/* The rich_location class within libcpp requires a way to expand
   location_t instances, and relies on the client code
   providing a symbol named
     linemap_client_expand_location_to_spelling_point
   to do this.

   This is the implementation for libcommon.a (all host binaries),
   which simply calls into expand_location_1.  */

expanded_location
linemap_client_expand_location_to_spelling_point (const line_maps *set,
						  location_t loc,
						  enum location_aspect aspect)
{
  return expand_location_1 (set, loc, /*expansion_point_p=*/false, aspect);
}


/* If LOCATION is in a system header and if it is a virtual location
   for a token coming from the expansion of a macro, unwind it to
   the location of the expansion point of the macro.  If the expansion
   point is also in a system header return the original LOCATION.
   Otherwise, return the location of the expansion point.

   This is used for instance when we want to emit diagnostics about a
   token that may be located in a macro that is itself defined in a
   system header, for example, for the NULL macro.  In such a case, if
   LOCATION were passed directly to diagnostic functions such as
   warning_at, the diagnostic would be suppressed (unless
   -Wsystem-headers).  */

location_t
expansion_point_location_if_in_system_header (location_t location)
{
  if (!in_system_header_at (location))
    return location;

  location_t xloc = linemap_resolve_location (line_table, location,
					      LRK_MACRO_EXPANSION_POINT,
					      NULL);
  return in_system_header_at (xloc) ? location : xloc;
}

/* If LOCATION is a virtual location for a token coming from the expansion
   of a macro, unwind to the location of the expansion point of the macro.  */

location_t
expansion_point_location (location_t location)
{
  return linemap_resolve_location (line_table, location,
				   LRK_MACRO_EXPANSION_POINT, NULL);
}

/* Construct a location with caret at CARET, ranging from START to
   FINISH.

   For example, consider:

                 11111111112
        12345678901234567890
     522
     523   return foo + bar;
                  ~~~~^~~~~
     524

   The location's caret is at the "+", line 523 column 15, but starts
   earlier, at the "f" of "foo" at column 11.  The finish is at the "r"
   of "bar" at column 19.  */

location_t
make_location (location_t caret, location_t start, location_t finish)
{
  return line_table->make_location (caret, start, finish);
}

/* Same as above, but taking a source range rather than two locations.  */

location_t
make_location (location_t caret, source_range src_range)
{
  location_t pure_loc = get_pure_location (caret);
  return line_table->get_or_create_combined_loc (pure_loc, src_range,
						 nullptr, 0);
}

/* An expanded_location stores the column in byte units.  This function
   converts that column to display units.  That requires reading the associated
   source line in order to calculate the display width.  If that cannot be done
   for any reason, then returns the byte column as a fallback.  */
int
location_compute_display_column (file_cache &fc,
				 expanded_location exploc,
				 const cpp_char_column_policy &policy)
{
  if (!(exploc.file && *exploc.file && exploc.line && exploc.column))
    return exploc.column;
  char_span line = fc.get_source_line (exploc.file, exploc.line);
  /* If line is NULL, this function returns exploc.column which is the
     desired fallback.  */
  return cpp_byte_column_to_display_column (line.get_buffer (), line.length (),
					    exploc.column, policy);
}

/* Dump statistics to stderr about the memory usage of the line_table
   set of line maps.  This also displays some statistics about macro
   expansion.  */

void
dump_line_table_statistics (void)
{
  struct linemap_stats s;
  long total_used_map_size,
    macro_maps_size,
    total_allocated_map_size;

  memset (&s, 0, sizeof (s));

  linemap_get_statistics (line_table, &s);

  macro_maps_size = s.macro_maps_used_size
    + s.macro_maps_locations_size;

  total_allocated_map_size = s.ordinary_maps_allocated_size
    + s.macro_maps_allocated_size
    + s.macro_maps_locations_size;

  total_used_map_size = s.ordinary_maps_used_size
    + s.macro_maps_used_size
    + s.macro_maps_locations_size;

  fprintf (stderr, "Number of expanded macros:                     %5ld\n",
           s.num_expanded_macros);
  if (s.num_expanded_macros != 0)
    fprintf (stderr, "Average number of tokens per macro expansion:  %5ld\n",
             s.num_macro_tokens / s.num_expanded_macros);
  fprintf (stderr,
           "\nLine Table allocations during the "
	   "compilation process\n");
  fprintf (stderr, "Number of ordinary maps used:        " PRsa (5) "\n",
	   SIZE_AMOUNT (s.num_ordinary_maps_used));
  fprintf (stderr, "Ordinary map used size:              " PRsa (5) "\n",
	   SIZE_AMOUNT (s.ordinary_maps_used_size));
  fprintf (stderr, "Number of ordinary maps allocated:   " PRsa (5) "\n",
	   SIZE_AMOUNT (s.num_ordinary_maps_allocated));
  fprintf (stderr, "Ordinary maps allocated size:        " PRsa (5) "\n",
	   SIZE_AMOUNT (s.ordinary_maps_allocated_size));
  fprintf (stderr, "Number of macro maps used:           " PRsa (5) "\n",
	   SIZE_AMOUNT (s.num_macro_maps_used));
  fprintf (stderr, "Macro maps used size:                " PRsa (5) "\n",
	   SIZE_AMOUNT (s.macro_maps_used_size));
  fprintf (stderr, "Macro maps locations size:           " PRsa (5) "\n",
	   SIZE_AMOUNT (s.macro_maps_locations_size));
  fprintf (stderr, "Macro maps size:                     " PRsa (5) "\n",
	   SIZE_AMOUNT (macro_maps_size));
  fprintf (stderr, "Duplicated maps locations size:      " PRsa (5) "\n",
	   SIZE_AMOUNT (s.duplicated_macro_maps_locations_size));
  fprintf (stderr, "Total allocated maps size:           " PRsa (5) "\n",
	   SIZE_AMOUNT (total_allocated_map_size));
  fprintf (stderr, "Total used maps size:                " PRsa (5) "\n",
	   SIZE_AMOUNT (total_used_map_size));
  fprintf (stderr, "Ad-hoc table size:                   " PRsa (5) "\n",
	   SIZE_AMOUNT (s.adhoc_table_size));
  fprintf (stderr, "Ad-hoc table entries used:           " PRsa (5) "\n",
	   SIZE_AMOUNT (s.adhoc_table_entries_used));
  fprintf (stderr, "optimized_ranges:                    " PRsa (5) "\n",
	   SIZE_AMOUNT (line_table->m_num_optimized_ranges));
  fprintf (stderr, "unoptimized_ranges:                  " PRsa (5) "\n",
	   SIZE_AMOUNT (line_table->m_num_unoptimized_ranges));

  fprintf (stderr, "\n");
}

/* Get location one beyond the final location in ordinary map IDX.  */

static location_t
get_end_location (class line_maps *set, line_map_uint_t idx)
{
  if (idx == LINEMAPS_ORDINARY_USED (set) - 1)
    return set->highest_location;

  struct line_map *next_map = LINEMAPS_ORDINARY_MAP_AT (set, idx + 1);
  return MAP_START_LOCATION (next_map);
}

/* Helper function for write_digit_row.  */

static void
write_digit (FILE *stream, int digit)
{
  fputc ('0' + (digit % 10), stream);
}

/* Helper function for dump_location_info.
   Write a row of numbers to STREAM, numbering a source line,
   giving the units, tens, hundreds etc of the column number.  */

static void
write_digit_row (FILE *stream, int indent,
		 const line_map_ordinary *map,
		 location_t loc, int max_col, int divisor)
{
  fprintf (stream, "%*c", indent, ' ');
  fprintf (stream, "|");
  for (int column = 1; column < max_col; column++)
    {
      location_t column_loc = loc + (location_t (column) << map->m_range_bits);
      write_digit (stream, column_loc / divisor);
    }
  fprintf (stream, "\n");
}

/* Write a half-closed (START) / half-open (END) interval of
   location_t to STREAM.  */

static void
dump_location_range (FILE *stream,
		     location_t start, location_t end)
{
  fprintf (stream,
	   "  location_t interval: %llu <= loc < %llu\n",
	   (unsigned long long) start, (unsigned long long) end);
}

/* Write a labelled description of a half-closed (START) / half-open (END)
   interval of location_t to STREAM.  */

static void
dump_labelled_location_range (FILE *stream,
			      const char *name,
			      location_t start, location_t end)
{
  fprintf (stream, "%s\n", name);
  dump_location_range (stream, start, end);
  fprintf (stream, "\n");
}

/* Write a visualization of the locations in the line_table to STREAM.  */

void
dump_location_info (FILE *stream)
{
  file_cache fc;

  /* Visualize the reserved locations.  */
  dump_labelled_location_range (stream, "RESERVED LOCATIONS",
				0, RESERVED_LOCATION_COUNT);

  using ULL = unsigned long long;

  /* Visualize the ordinary line_map instances, rendering the sources. */
  for (line_map_uint_t idx = 0; idx < LINEMAPS_ORDINARY_USED (line_table);
       idx++)
    {
      location_t end_location = get_end_location (line_table, idx);
      /* half-closed: doesn't include this one. */

      const line_map_ordinary *map
	= LINEMAPS_ORDINARY_MAP_AT (line_table, idx);
      fprintf (stream, "ORDINARY MAP: %llu\n", (ULL) idx);
      dump_location_range (stream,
			   MAP_START_LOCATION (map), end_location);
      fprintf (stream, "  file: %s\n", ORDINARY_MAP_FILE_NAME (map));
      fprintf (stream, "  starting at line: %i\n",
	       ORDINARY_MAP_STARTING_LINE_NUMBER (map));
      fprintf (stream, "  column and range bits: %i\n",
	       map->m_column_and_range_bits);
      fprintf (stream, "  column bits: %i\n",
	       map->m_column_and_range_bits - map->m_range_bits);
      fprintf (stream, "  range bits: %i\n",
	       map->m_range_bits);
      const char * reason;
      switch (map->reason) {
      case LC_ENTER:
	reason = "LC_ENTER";
	break;
      case LC_LEAVE:
	reason = "LC_LEAVE";
	break;
      case LC_RENAME:
	reason = "LC_RENAME";
	break;
      case LC_RENAME_VERBATIM:
	reason = "LC_RENAME_VERBATIM";
	break;
      case LC_ENTER_MACRO:
	reason = "LC_RENAME_MACRO";
	break;
      default:
	reason = "Unknown";
      }
      fprintf (stream, "  reason: %d (%s)\n", map->reason, reason);

      const line_map_ordinary *includer_map
	= linemap_included_from_linemap (line_table, map);
      fprintf (stream, "  included from location: %llu",
	       (ULL) linemap_included_from (map));
      if (includer_map) {
	fprintf (stream, " (in ordinary map %llu)",
		 ULL (includer_map - line_table->info_ordinary.maps));
      }
      fprintf (stream, "\n");

      /* Render the span of source lines that this "map" covers.  */
      for (location_t loc = MAP_START_LOCATION (map);
	   loc < end_location;
	   loc += (location_t (1) << map->m_range_bits))
	{
	  gcc_assert (pure_location_p (line_table, loc) );

	  expanded_location exploc
	    = linemap_expand_location (line_table, map, loc);

	  if (exploc.column == 0)
	    {
	      /* Beginning of a new source line: draw the line.  */

	      char_span line_text = fc.get_source_line (exploc.file,
							exploc.line);
	      if (!line_text)
		break;
	      fprintf (stream,
		       "%s:%3i|loc:%5llu|%.*s\n",
		       exploc.file, exploc.line,
		       (ULL) loc,
		       (int)line_text.length (), line_text.get_buffer ());

	      /* "loc" is at column 0, which means "the whole line".
		 Render the locations *within* the line, by underlining
		 it, showing the location_t numeric values
		 at each column.  */
	      auto max_col = (ULL (1) << map->m_column_and_range_bits) - 1;
	      if (max_col > line_text.length ())
		max_col = line_text.length () + 1;

	      int len_lnum = num_digits (exploc.line);
	      if (len_lnum < 3)
		len_lnum = 3;
	      int len_loc = num_digits (loc);
	      if (len_loc < 5)
		len_loc = 5;

	      int indent = 6 + strlen (exploc.file) + len_lnum + len_loc;

	      /* Thousands.  */
	      if (end_location > 999)
		write_digit_row (stream, indent, map, loc, max_col, 1000);

	      /* Hundreds.  */
	      if (end_location > 99)
		write_digit_row (stream, indent, map, loc, max_col, 100);

	      /* Tens.  */
	      write_digit_row (stream, indent, map, loc, max_col, 10);

	      /* Units.  */
	      write_digit_row (stream, indent, map, loc, max_col, 1);
	    }
	}
      fprintf (stream, "\n");
    }

  /* Visualize unallocated values.  */
  dump_labelled_location_range (stream, "UNALLOCATED LOCATIONS",
				line_table->highest_location,
				LINEMAPS_MACRO_LOWEST_LOCATION (line_table));

  /* Visualize the macro line_map instances, rendering the sources. */
  for (line_map_uint_t i = 0; i < LINEMAPS_MACRO_USED (line_table); i++)
    {
      /* Each macro map that is allocated owns location_t values
	 that are *lower* that the one before them.
	 Hence it's meaningful to view them either in order of ascending
	 source locations, or in order of ascending macro map index.  */
      const bool ascending_location_ts = true;
      auto idx = (ascending_location_ts
		  ? (LINEMAPS_MACRO_USED (line_table) - (i + 1))
		  : i);
      const line_map_macro *map = LINEMAPS_MACRO_MAP_AT (line_table, idx);
      fprintf (stream, "MACRO %llu: %s (%u tokens)\n",
	       (ULL) idx,
	       linemap_map_get_macro_name (map),
	       MACRO_MAP_NUM_MACRO_TOKENS (map));
      dump_location_range (stream,
			   map->start_location,
			   (map->start_location
			    + MACRO_MAP_NUM_MACRO_TOKENS (map)));
      inform (map->get_expansion_point_location (),
	      "expansion point is location %llu",
	      (ULL) map->get_expansion_point_location ());
      fprintf (stream, "  map->start_location: %llu\n",
	       (ULL) map->start_location);

      fprintf (stream, "  macro_locations:\n");
      for (unsigned int i = 0; i < MACRO_MAP_NUM_MACRO_TOKENS (map); i++)
	{
	  location_t x = MACRO_MAP_LOCATIONS (map)[2 * i];
	  location_t y = MACRO_MAP_LOCATIONS (map)[(2 * i) + 1];

	  /* linemap_add_macro_token encodes token numbers in an expansion
	     by putting them after MAP_START_LOCATION. */

	  /* I'm typically seeing 4 uninitialized entries at the end of
	     0xafafafaf.
	     This appears to be due to macro.cc:replace_args
	     adding 2 extra args for padding tokens; presumably there may
	     be a leading and/or trailing padding token injected,
	     each for 2 more location slots.
	     This would explain there being up to 4 location_ts slots
	     that may be uninitialized.  */

	  fprintf (stream, "    %u: %llu, %llu\n",
		   i,
		   (ULL) x,
		   (ULL) y);
	  if (x == y)
	    {
	      if (x < MAP_START_LOCATION (map))
		inform (x, "token %u has %<x-location == y-location == %llu%>",
			i, (ULL) x);
	      else
		fprintf (stream,
			 "x-location == y-location == %llu"
			 " encodes token # %u\n",
			 (ULL) x,
			 (unsigned int)(x - MAP_START_LOCATION (map)));
	    }
	  else
	    {
	      inform (x, "token %u has %<x-location == %llu%>", i, (ULL) x);
	      inform (x, "token %u has %<y-location == %llu%>", i, (ULL) y);
	    }
	}
      fprintf (stream, "\n");
    }

  /* It appears that MAX_LOCATION_T itself is never assigned to a
     macro map, presumably due to an off-by-one error somewhere
     between the logic in linemap_enter_macro and
     LINEMAPS_MACRO_LOWEST_LOCATION.  */
  dump_labelled_location_range (stream, "MAX_LOCATION_T",
				MAX_LOCATION_T,
				MAX_LOCATION_T + 1);

  /* Visualize ad-hoc values.  */
  dump_labelled_location_range (stream, "AD-HOC LOCATIONS",
				MAX_LOCATION_T + 1, location_t (-1));
}

/* string_concat's constructor.  */

string_concat::string_concat (int num, location_t *locs)
  : m_num (num)
{
  m_locs = ggc_vec_alloc <location_t> (num);
  for (int i = 0; i < num; i++)
    m_locs[i] = locs[i];
}

/* string_concat_db's constructor.  */

string_concat_db::string_concat_db ()
{
  m_table = hash_map <location_hash, string_concat *>::create_ggc (64);
}

/* Record that a string concatenation occurred, covering NUM
   string literal tokens.  LOCS is an array of size NUM, containing the
   locations of the tokens.  A copy of LOCS is taken.  */

void
string_concat_db::record_string_concatenation (int num, location_t *locs)
{
  gcc_assert (num > 1);
  gcc_assert (locs);

  location_t key_loc = get_key_loc (locs[0]);
  /* We don't record data for 'RESERVED_LOCATION_P (key_loc)' key values:
     any data now recorded under key 'key_loc' would be overwritten by a
     subsequent call with the same key 'key_loc'.  */
  if (RESERVED_LOCATION_P (key_loc))
    return;

  string_concat *concat
    = new (ggc_alloc <string_concat> ()) string_concat (num, locs);
  m_table->put (key_loc, concat);
}

/* Determine if LOC was the location of the initial token of a
   concatenation of string literal tokens.
   If so, *OUT_NUM is written to with the number of tokens, and
   *OUT_LOCS with the location of an array of locations of the
   tokens, and return true.  *OUT_LOCS is a borrowed pointer to
   storage owned by the string_concat_db.
   Otherwise, return false.  */

bool
string_concat_db::get_string_concatenation (location_t loc,
					    int *out_num,
					    location_t **out_locs)
{
  gcc_assert (out_num);
  gcc_assert (out_locs);

  location_t key_loc = get_key_loc (loc);
  /* We don't record data for 'RESERVED_LOCATION_P (key_loc)' key values; see
     discussion in 'string_concat_db::record_string_concatenation'.  */
  if (RESERVED_LOCATION_P (key_loc))
    return false;

  string_concat **concat = m_table->get (key_loc);
  if (!concat)
    return false;

  *out_num = (*concat)->m_num;
  *out_locs =(*concat)->m_locs;
  return true;
}

/* Internal function.  Canonicalize LOC into a form suitable for
   use as a key within the database, stripping away macro expansion,
   ad-hoc information, and range information, using the location of
   the start of LOC within an ordinary linemap.  */

location_t
string_concat_db::get_key_loc (location_t loc)
{
  loc = linemap_resolve_location (line_table, loc, LRK_SPELLING_LOCATION,
				  NULL);

  loc = get_range_from_loc (line_table, loc).m_start;

  return loc;
}

/* Helper class for use within get_substring_ranges_for_loc.
   An vec of cpp_string with responsibility for releasing all of the
   str->text for each str in the vector.  */

class auto_cpp_string_vec :  public auto_vec <cpp_string>
{
 public:
  auto_cpp_string_vec (int alloc)
    : auto_vec <cpp_string> (alloc) {}

  ~auto_cpp_string_vec ()
  {
    /* Clean up the copies within this vec.  */
    int i;
    cpp_string *str;
    FOR_EACH_VEC_ELT (*this, i, str)
      free (const_cast <unsigned char *> (str->text));
  }
};

/* Attempt to populate RANGES with source location information on the
   individual characters within the string literal found at STRLOC.
   If CONCATS is non-NULL, then any string literals that the token at
   STRLOC  was concatenated with are also added to RANGES.

   Return NULL if successful, or an error message if any errors occurred (in
   which case RANGES may be only partially populated and should not
   be used).

   This is implemented by re-parsing the relevant source line(s).  */

static const char *
get_substring_ranges_for_loc (cpp_reader *pfile,
			      file_cache &fc,
			      string_concat_db *concats,
			      location_t strloc,
			      enum cpp_ttype type,
			      cpp_substring_ranges &ranges)
{
  gcc_assert (pfile);

  if (strloc == UNKNOWN_LOCATION)
    return "unknown location";

  /* Reparsing the strings requires accurate location information.
     If -ftrack-macro-expansion has been overridden from its default
     of 2, then we might have a location of a macro expansion point,
     rather than the location of the literal itself.
     Avoid this by requiring that we have full macro expansion tracking
     for substring locations to be available.  */
  if (cpp_get_options (pfile)->track_macro_expansion != 2)
    return "track_macro_expansion != 2";

  /* If #line or # 44 "file"-style directives are present, then there's
     no guarantee that the line numbers we have can be used to locate
     the strings.  For example, we might have a .i file with # directives
     pointing back to lines within a .c file, but the .c file might
     have been edited since the .i file was created.
     In such a case, the safest course is to disable on-demand substring
     locations.  */
  if (line_table->seen_line_directive)
    return "seen line directive";

  /* If string concatenation has occurred at STRLOC, get the locations
     of all of the literal tokens making up the compound string.
     Otherwise, just use STRLOC.  */
  int num_locs = 1;
  location_t *strlocs = &strloc;
  if (concats)
    concats->get_string_concatenation (strloc, &num_locs, &strlocs);

  auto_cpp_string_vec strs (num_locs);
  auto_vec <cpp_string_location_reader> loc_readers (num_locs);
  for (int i = 0; i < num_locs; i++)
    {
      /* Get range of strloc.  We will use it to locate the start and finish
	 of the literal token within the line.  */
      source_range src_range = get_range_from_loc (line_table, strlocs[i]);

      if (src_range.m_start >= LINEMAPS_MACRO_LOWEST_LOCATION (line_table))
	{
	  /* If the string token was within a macro expansion, then we can
	     cope with it for the simple case where we have a single token.
	     Otherwise, bail out.  */
	  if (src_range.m_start != src_range.m_finish)
	    return "macro expansion";
	}
      else
	{
	  if (src_range.m_start >= LINE_MAP_MAX_LOCATION_WITH_COLS)
	    /* If so, we can't reliably determine where the token started within
	       its line.  */
	    return "range starts after LINE_MAP_MAX_LOCATION_WITH_COLS";

	  if (src_range.m_finish >= LINE_MAP_MAX_LOCATION_WITH_COLS)
	    /* If so, we can't reliably determine where the token finished
	       within its line.  */
	    return "range ends after LINE_MAP_MAX_LOCATION_WITH_COLS";
	}

      expanded_location start
	= expand_location_to_spelling_point (src_range.m_start,
					     LOCATION_ASPECT_START);
      expanded_location finish
	= expand_location_to_spelling_point (src_range.m_finish,
					     LOCATION_ASPECT_FINISH);
      if (start.file != finish.file)
	return "range endpoints are in different files";
      if (start.line != finish.line)
	return "range endpoints are on different lines";
      if (start.column > finish.column)
	return "range endpoints are reversed";

      char_span line = fc.get_source_line (start.file, start.line);
      if (!line)
	return "unable to read source line";

      /* Determine the location of the literal (including quotes
	 and leading prefix chars, such as the 'u' in a u""
	 token).  */
      size_t literal_length = finish.column - start.column + 1;

      /* Ensure that we don't crash if we got the wrong location.  */
      if (start.column < 1)
	return "zero start column";
      if (line.length () < (start.column - 1 + literal_length))
	return "line is not wide enough";

      char_span literal = line.subspan (start.column - 1, literal_length);

      cpp_string from;
      from.len = literal_length;
      /* Make a copy of the literal, to avoid having to rely on
	 the lifetime of the copy of the line within the cache.
	 This will be released by the auto_cpp_string_vec dtor.  */
      from.text = (unsigned char *)literal.xstrdup ();
      strs.safe_push (from);

      /* For very long lines, a new linemap could have started
	 halfway through the token.
	 Ensure that the loc_reader uses the linemap of the
	 *end* of the token for its start location.  */
      const line_map_ordinary *start_ord_map;
      linemap_resolve_location (line_table, src_range.m_start,
				LRK_SPELLING_LOCATION, &start_ord_map);
      const line_map_ordinary *final_ord_map;
      linemap_resolve_location (line_table, src_range.m_finish,
				LRK_SPELLING_LOCATION, &final_ord_map);
      if (start_ord_map == NULL || final_ord_map == NULL)
	return "failed to get ordinary maps";
      /* Bulletproofing.  We ought to only have different ordinary maps
	 for start vs finish due to line-length jumps.  */
      if (start_ord_map != final_ord_map
	  && start_ord_map->to_file != final_ord_map->to_file)
	return "start and finish are spelled in different ordinary maps";
      /* The file from linemap_resolve_location ought to match that from
	 expand_location_to_spelling_point.  */
      if (start_ord_map->to_file != start.file)
	return "mismatching file after resolving linemap";

      location_t start_loc
	= linemap_position_for_line_and_column (line_table, final_ord_map,
						start.line, start.column);

      cpp_string_location_reader loc_reader (start_loc, line_table);
      loc_readers.safe_push (loc_reader);
    }

  /* Rerun cpp_interpret_string, or rather, a modified version of it.  */
  const char *err = cpp_interpret_string_ranges (pfile, strs.address (),
						 loc_readers.address (),
						 num_locs, &ranges, type);
  if (err)
    return err;

  /* Success: "ranges" should now contain information on the string.  */
  return NULL;
}

/* Attempt to populate *OUT_LOC with source location information on the
   given characters within the string literal found at STRLOC.
   CARET_IDX, START_IDX, and END_IDX refer to offsets within the execution
   character set.

   For example, given CARET_IDX = 4, START_IDX = 3, END_IDX  = 7
   and string literal "012345\n789"
   *OUT_LOC is written to with:
     "012345\n789"
         ~^~~~~

   If CONCATS is non-NULL, then any string literals that the token at
   STRLOC was concatenated with are also considered.

   This is implemented by re-parsing the relevant source line(s).

   Return NULL if successful, or an error message if any errors occurred.
   Error messages are intended for GCC developers (to help debugging) rather
   than for end-users.  */

const char *
get_location_within_string (cpp_reader *pfile,
			    file_cache &fc,
			    string_concat_db *concats,
			    location_t strloc,
			    enum cpp_ttype type,
			    int caret_idx, int start_idx, int end_idx,
			    location_t *out_loc)
{
  gcc_checking_assert (caret_idx >= 0);
  gcc_checking_assert (start_idx >= 0);
  gcc_checking_assert (end_idx >= 0);
  gcc_assert (out_loc);

  cpp_substring_ranges ranges;
  const char *err
    = get_substring_ranges_for_loc (pfile, fc, concats, strloc, type, ranges);
  if (err)
    return err;

  if (caret_idx >= ranges.get_num_ranges ())
    return "caret_idx out of range";
  if (start_idx >= ranges.get_num_ranges ())
    return "start_idx out of range";
  if (end_idx >= ranges.get_num_ranges ())
    return "end_idx out of range";

  *out_loc = make_location (ranges.get_range (caret_idx).m_start,
			    ranges.get_range (start_idx).m_start,
			    ranges.get_range (end_idx).m_finish);
  return NULL;
}

/* Associate the DISCRIMINATOR with LOCUS, and return a new locus. */

location_t
location_with_discriminator (location_t locus, int discriminator)
{
  tree block = LOCATION_BLOCK (locus);
  source_range src_range = get_range_from_loc (line_table, locus);
  locus = get_pure_location (locus);

  if (locus == UNKNOWN_LOCATION)
    return locus;

  return line_table->get_or_create_combined_loc (locus, src_range, block,
						 discriminator);
}

/* Return TRUE if LOCUS represents a location with a discriminator.  */

bool
has_discriminator (location_t locus)
{
  return get_discriminator_from_loc (locus) != 0;
}

/* Return the discriminator for LOCUS.  */

int
get_discriminator_from_loc (location_t locus)
{
  return get_discriminator_from_loc (line_table, locus);
}

#if CHECKING_P

namespace selftest {

/* Selftests of location handling.  */

/* Attempt to populate *OUT_RANGE with source location information on the
   given character within the string literal found at STRLOC.
   CHAR_IDX refers to an offset within the execution character set.
   If CONCATS is non-NULL, then any string literals that the token at
   STRLOC was concatenated with are also considered.

   This is implemented by re-parsing the relevant source line(s).

   Return NULL if successful, or an error message if any errors occurred.
   Error messages are intended for GCC developers (to help debugging) rather
   than for end-users.  */

static const char *
get_source_range_for_char (cpp_reader *pfile,
			   file_cache &fc,
			   string_concat_db *concats,
			   location_t strloc,
			   enum cpp_ttype type,
			   int char_idx,
			   source_range *out_range)
{
  gcc_checking_assert (char_idx >= 0);
  gcc_assert (out_range);

  cpp_substring_ranges ranges;
  const char *err
    = get_substring_ranges_for_loc (pfile, fc, concats, strloc, type, ranges);
  if (err)
    return err;

  if (char_idx >= ranges.get_num_ranges ())
    return "char_idx out of range";

  *out_range = ranges.get_range (char_idx);
  return NULL;
}

/* As get_source_range_for_char, but write to *OUT the number
   of ranges that are available.  */

static const char *
get_num_source_ranges_for_substring (cpp_reader *pfile,
				     file_cache &fc,
				     string_concat_db *concats,
				     location_t strloc,
				     enum cpp_ttype type,
				     int *out)
{
  gcc_assert (out);

  cpp_substring_ranges ranges;
  const char *err
    = get_substring_ranges_for_loc (pfile, fc, concats, strloc, type, ranges);

  if (err)
    return err;

  *out = ranges.get_num_ranges ();
  return NULL;
}

/* Selftests of location handling.  */

/* Verify that compare() on linenum_type handles comparisons over the full
   range of the type.  */

static void
test_linenum_comparisons ()
{
  linenum_type min_line (0);
  linenum_type max_line (0xffffffff);
  ASSERT_EQ (0, compare (min_line, min_line));
  ASSERT_EQ (0, compare (max_line, max_line));

  ASSERT_GT (compare (max_line, min_line), 0);
  ASSERT_LT (compare (min_line, max_line), 0);
}

/* Helper function for verifying location data: when location_t
   values are > LINE_MAP_MAX_LOCATION_WITH_COLS, they are treated
   as having column 0.  */

static bool
should_have_column_data_p (location_t loc)
{
  if (IS_ADHOC_LOC (loc))
    loc = get_location_from_adhoc_loc (line_table, loc);
  if (loc > LINE_MAP_MAX_LOCATION_WITH_COLS)
    return false;
  return true;
}

/* Selftest for should_have_column_data_p.  */

static void
test_should_have_column_data_p ()
{
  ASSERT_TRUE (should_have_column_data_p (RESERVED_LOCATION_COUNT));
  ASSERT_TRUE
    (should_have_column_data_p (LINE_MAP_MAX_LOCATION_WITH_COLS));
  ASSERT_FALSE
    (should_have_column_data_p (LINE_MAP_MAX_LOCATION_WITH_COLS + 1));
}

/* Verify the result of LOCATION_FILE/LOCATION_LINE/LOCATION_COLUMN
   on LOC.  */

static void
assert_loceq (const char *exp_filename, int exp_linenum, int exp_colnum,
	      location_t loc)
{
  ASSERT_STREQ (exp_filename, LOCATION_FILE (loc));
  ASSERT_EQ (exp_linenum, LOCATION_LINE (loc));
  /* If location_t values are sufficiently high, then column numbers
     will be unavailable and LOCATION_COLUMN (loc) will be 0.
     When close to the threshold, column numbers *may* be present: if
     the final linemap before the threshold contains a line that straddles
     the threshold, locations in that line have column information.  */
  if (should_have_column_data_p (loc))
    ASSERT_EQ (exp_colnum, LOCATION_COLUMN (loc));
}

/* Various selftests involve constructing a line table and one or more
   line maps within it.

   For maximum test coverage we want to run these tests with a variety
   of situations:
   - line_table->default_range_bits: some frontends use a non-zero value
   and others use zero
   - the fallback modes within line-map.cc: there are various threshold
   values for location_t beyond line-map.cc changes
   behavior (disabling of the range-packing optimization, disabling
   of column-tracking).  We can exercise these by starting the line_table
   at interesting values at or near these thresholds.

   The following struct describes a particular case within our test
   matrix.  */

class line_table_case
{
public:
  line_table_case (int default_range_bits, location_t base_location)
  : m_default_range_bits (default_range_bits),
    m_base_location (base_location)
  {}

  int m_default_range_bits;
  location_t m_base_location;
};

/* Constructor.  Store the old value of line_table, and create a new
   one, using sane defaults.  */

line_table_test::line_table_test ()
{
  gcc_assert (saved_line_table == NULL);
  saved_line_table = line_table;
  line_table = ggc_alloc<line_maps> ();
  linemap_init (line_table, BUILTINS_LOCATION);
  gcc_assert (saved_line_table->m_reallocator);
  line_table->m_reallocator = saved_line_table->m_reallocator;
  gcc_assert (saved_line_table->m_round_alloc_size);
  line_table->m_round_alloc_size = saved_line_table->m_round_alloc_size;
  line_table->default_range_bits = 0;
}

/* Constructor.  Store the old value of line_table, and create a new
   one, using the sitation described in CASE_.  */

line_table_test::line_table_test (const line_table_case &case_)
{
  gcc_assert (saved_line_table == NULL);
  saved_line_table = line_table;
  line_table = ggc_alloc<line_maps> ();
  linemap_init (line_table, BUILTINS_LOCATION);
  gcc_assert (saved_line_table->m_reallocator);
  line_table->m_reallocator = saved_line_table->m_reallocator;
  gcc_assert (saved_line_table->m_round_alloc_size);
  line_table->m_round_alloc_size = saved_line_table->m_round_alloc_size;
  line_table->default_range_bits = case_.m_default_range_bits;
  if (case_.m_base_location)
    {
      line_table->highest_location = case_.m_base_location;
      line_table->highest_line = case_.m_base_location;
    }
}

/* Destructor.  Restore the old value of line_table.  */

line_table_test::~line_table_test ()
{
  gcc_assert (saved_line_table != NULL);
  line_table = saved_line_table;
  saved_line_table = NULL;
}

/* Verify basic operation of ordinary linemaps.  */

static void
test_accessing_ordinary_linemaps (const line_table_case &case_)
{
  line_table_test ltt (case_);

  /* Build a simple linemap describing some locations. */
  linemap_add (line_table, LC_ENTER, false, "foo.c", 0);

  linemap_line_start (line_table, 1, 100);
  location_t loc_a = linemap_position_for_column (line_table, 1);
  location_t loc_b = linemap_position_for_column (line_table, 23);

  linemap_line_start (line_table, 2, 100);
  location_t loc_c = linemap_position_for_column (line_table, 1);
  location_t loc_d = linemap_position_for_column (line_table, 17);

  /* Example of a very long line.  */
  linemap_line_start (line_table, 3, 2000);
  location_t loc_e = linemap_position_for_column (line_table, 700);

  /* Transitioning back to a short line.  */
  linemap_line_start (line_table, 4, 0);
  location_t loc_back_to_short = linemap_position_for_column (line_table, 100);

  if (should_have_column_data_p (loc_back_to_short))
    {
      /* Verify that we switched to short lines in the linemap.  */
      line_map_ordinary *map = LINEMAPS_LAST_ORDINARY_MAP (line_table);
      ASSERT_EQ (7, map->m_column_and_range_bits - map->m_range_bits);
    }

  /* Example of a line that will eventually be seen to be longer
     than LINE_MAP_MAX_COLUMN_NUMBER; the initially seen width is
     below that.  */
  linemap_line_start (line_table, 5, 2000);

  location_t loc_start_of_very_long_line
    = linemap_position_for_column (line_table, 2000);
  location_t loc_too_wide
    = linemap_position_for_column (line_table, LINE_MAP_MAX_COLUMN_NUMBER + 1);
  location_t loc_too_wide_2
    = linemap_position_for_column (line_table, LINE_MAP_MAX_COLUMN_NUMBER + 2);

  /* ...and back to a sane line length.  */
  linemap_line_start (line_table, 6, 100);
  location_t loc_sane_again = linemap_position_for_column (line_table, 10);

  linemap_add (line_table, LC_LEAVE, false, NULL, 0);

  /* Multiple files.  */
  linemap_add (line_table, LC_ENTER, false, "bar.c", 0);
  linemap_line_start (line_table, 1, 200);
  location_t loc_f = linemap_position_for_column (line_table, 150);
  linemap_add (line_table, LC_LEAVE, false, NULL, 0);

  /* Verify that we can recover the location info.  */
  assert_loceq ("foo.c", 1, 1, loc_a);
  assert_loceq ("foo.c", 1, 23, loc_b);
  assert_loceq ("foo.c", 2, 1, loc_c);
  assert_loceq ("foo.c", 2, 17, loc_d);
  assert_loceq ("foo.c", 3, 700, loc_e);
  assert_loceq ("foo.c", 4, 100, loc_back_to_short);

  /* In the very wide line, the initial location should be fully tracked.  */
  assert_loceq ("foo.c", 5, 2000, loc_start_of_very_long_line);
  /* ...but once we exceed LINE_MAP_MAX_COLUMN_NUMBER column-tracking should
     be disabled.  */
  assert_loceq ("foo.c", 5, 0, loc_too_wide);
  assert_loceq ("foo.c", 5, 0, loc_too_wide_2);
  /*...and column-tracking should be re-enabled for subsequent lines.  */
  assert_loceq ("foo.c", 6, 10, loc_sane_again);

  assert_loceq ("bar.c", 1, 150, loc_f);

  ASSERT_FALSE (is_location_from_builtin_token (loc_a));
  ASSERT_TRUE (pure_location_p (line_table, loc_a));

  /* Verify using make_location to build a range, and extracting data
     back from it.  */
  location_t range_c_b_d = make_location (loc_c, loc_b, loc_d);
  ASSERT_FALSE (pure_location_p (line_table, range_c_b_d));
  ASSERT_EQ (loc_c, get_location_from_adhoc_loc (line_table, range_c_b_d));
  source_range src_range = get_range_from_loc (line_table, range_c_b_d);
  ASSERT_EQ (loc_b, src_range.m_start);
  ASSERT_EQ (loc_d, src_range.m_finish);
}

/* Verify various properties of UNKNOWN_LOCATION.  */

static void
test_unknown_location ()
{
  ASSERT_EQ (NULL, LOCATION_FILE (UNKNOWN_LOCATION));
  ASSERT_EQ (0, LOCATION_LINE (UNKNOWN_LOCATION));
  ASSERT_EQ (0, LOCATION_COLUMN (UNKNOWN_LOCATION));
}

/* Verify various properties of BUILTINS_LOCATION.  */

static void
test_builtins ()
{
  assert_loceq (special_fname_builtin (), 0, 0, BUILTINS_LOCATION);
  ASSERT_PRED1 (is_location_from_builtin_token, BUILTINS_LOCATION);
}

/* Regression test for make_location.
   Ensure that we use pure locations for the start/finish of the range,
   rather than storing a packed or ad-hoc range as the start/finish.  */

static void
test_make_location_nonpure_range_endpoints (const line_table_case &case_)
{
  /* Issue seen with testsuite/c-c++-common/Wlogical-not-parentheses-2.c
     with C++ frontend.
     ....................0000000001111111111222.
     ....................1234567890123456789012.  */
  const char *content = "     r += !aaa == bbb;\n";
  temp_source_file tmp (SELFTEST_LOCATION, ".C", content);
  line_table_test ltt (case_);
  linemap_add (line_table, LC_ENTER, false, tmp.get_filename (), 1);

  const location_t c11 = linemap_position_for_column (line_table, 11);
  const location_t c12 = linemap_position_for_column (line_table, 12);
  const location_t c13 = linemap_position_for_column (line_table, 13);
  const location_t c14 = linemap_position_for_column (line_table, 14);
  const location_t c21 = linemap_position_for_column (line_table, 21);

  if (c21 > LINE_MAP_MAX_LOCATION_WITH_COLS)
    return;

  /* Use column 13 for the caret location, arbitrarily, to verify that we
     handle start != caret.  */
  const location_t aaa = make_location (c13, c12, c14);
  ASSERT_EQ (c13, get_pure_location (aaa));
  ASSERT_EQ (c12, get_start (aaa));
  ASSERT_FALSE (IS_ADHOC_LOC (get_start (aaa)));
  ASSERT_EQ (c14, get_finish (aaa));
  ASSERT_FALSE (IS_ADHOC_LOC (get_finish (aaa)));

  /* Make a location using a location with a range as the start-point.  */
  const location_t not_aaa = make_location (c11, aaa, c14);
  ASSERT_EQ (c11, get_pure_location (not_aaa));
  /* It should use the start location of the range, not store the range
     itself.  */
  ASSERT_EQ (c12, get_start (not_aaa));
  ASSERT_FALSE (IS_ADHOC_LOC (get_start (not_aaa)));
  ASSERT_EQ (c14, get_finish (not_aaa));
  ASSERT_FALSE (IS_ADHOC_LOC (get_finish (not_aaa)));

  /* Similarly, make a location with a range as the end-point.  */
  const location_t aaa_eq_bbb = make_location (c12, c12, c21);
  ASSERT_EQ (c12, get_pure_location (aaa_eq_bbb));
  ASSERT_EQ (c12, get_start (aaa_eq_bbb));
  ASSERT_FALSE (IS_ADHOC_LOC (get_start (aaa_eq_bbb)));
  ASSERT_EQ (c21, get_finish (aaa_eq_bbb));
  ASSERT_FALSE (IS_ADHOC_LOC (get_finish (aaa_eq_bbb)));
  const location_t not_aaa_eq_bbb = make_location (c11, c12, aaa_eq_bbb);
  /* It should use the finish location of the range, not store the range
     itself.  */
  ASSERT_EQ (c11, get_pure_location (not_aaa_eq_bbb));
  ASSERT_EQ (c12, get_start (not_aaa_eq_bbb));
  ASSERT_FALSE (IS_ADHOC_LOC (get_start (not_aaa_eq_bbb)));
  ASSERT_EQ (c21, get_finish (not_aaa_eq_bbb));
  ASSERT_FALSE (IS_ADHOC_LOC (get_finish (not_aaa_eq_bbb)));
}

/* Verify reading of input files (e.g. for caret-based diagnostics).  */

static void
test_reading_source_line ()
{
  /* Create a tempfile and write some text to it.  */
  temp_source_file tmp (SELFTEST_LOCATION, ".txt",
			"01234567890123456789\n"
			"This is the test text\n"
			"This is the 3rd line");
  file_cache fc;

  /* Read back a specific line from the tempfile.  */
  char_span source_line = fc.get_source_line (tmp.get_filename (), 3);
  ASSERT_TRUE (source_line);
  ASSERT_TRUE (source_line.get_buffer () != NULL);
  ASSERT_EQ (20, source_line.length ());
  ASSERT_TRUE (!strncmp ("This is the 3rd line",
			 source_line.get_buffer (), source_line.length ()));

  source_line = fc.get_source_line (tmp.get_filename (), 2);
  ASSERT_TRUE (source_line);
  ASSERT_TRUE (source_line.get_buffer () != NULL);
  ASSERT_EQ (21, source_line.length ());
  ASSERT_TRUE (!strncmp ("This is the test text",
			 source_line.get_buffer (), source_line.length ()));

  source_line = fc.get_source_line (tmp.get_filename (), 4);
  ASSERT_FALSE (source_line);
  ASSERT_TRUE (source_line.get_buffer () == NULL);
}

/* Verify reading from buffers (e.g. for sarif-replay).  */

static void
test_reading_source_buffer ()
{
  const char *text = ("01234567890123456789\n"
		      "This is the test text\n"
		      "This is the 3rd line");
  const char *filename = "foo.txt";
  file_cache fc;
  fc.add_buffered_content (filename, text, strlen (text));

  /* Read back a specific line from the tempfile.  */
  char_span source_line = fc.get_source_line (filename, 3);
  ASSERT_TRUE (source_line);
  ASSERT_TRUE (source_line.get_buffer () != NULL);
  ASSERT_EQ (20, source_line.length ());
  ASSERT_TRUE (!strncmp ("This is the 3rd line",
			 source_line.get_buffer (), source_line.length ()));

  source_line = fc.get_source_line (filename, 2);
  ASSERT_TRUE (source_line);
  ASSERT_TRUE (source_line.get_buffer () != NULL);
  ASSERT_EQ (21, source_line.length ());
  ASSERT_TRUE (!strncmp ("This is the test text",
			 source_line.get_buffer (), source_line.length ()));

  source_line = fc.get_source_line (filename, 4);
  ASSERT_FALSE (source_line);
  ASSERT_TRUE (source_line.get_buffer () == NULL);
}

/* Tests of lexing.  */

/* Verify that token TOK from PARSER has cpp_token_as_text
   equal to EXPECTED_TEXT.  */

#define ASSERT_TOKEN_AS_TEXT_EQ(PARSER, TOK, EXPECTED_TEXT)		\
  SELFTEST_BEGIN_STMT							\
    unsigned char *actual_txt = cpp_token_as_text ((PARSER), (TOK));	\
    ASSERT_STREQ ((EXPECTED_TEXT), (const char *)actual_txt);		\
  SELFTEST_END_STMT

/* Verify that TOK's src_loc is within EXP_FILENAME at EXP_LINENUM,
   and ranges from EXP_START_COL to EXP_FINISH_COL.
   Use LOC as the effective location of the selftest.  */

static void
assert_token_loc_eq (const location &loc,
		     const cpp_token *tok,
		     const char *exp_filename, int exp_linenum,
		     int exp_start_col, int exp_finish_col)
{
  location_t tok_loc = tok->src_loc;
  ASSERT_STREQ_AT (loc, exp_filename, LOCATION_FILE (tok_loc));
  ASSERT_EQ_AT (loc, exp_linenum, LOCATION_LINE (tok_loc));

  /* If location_t values are sufficiently high, then column numbers
     will be unavailable.  */
  if (!should_have_column_data_p (tok_loc))
    return;

  ASSERT_EQ_AT (loc, exp_start_col, LOCATION_COLUMN (tok_loc));
  source_range tok_range = get_range_from_loc (line_table, tok_loc);
  ASSERT_EQ_AT (loc, exp_start_col, LOCATION_COLUMN (tok_range.m_start));
  ASSERT_EQ_AT (loc, exp_finish_col, LOCATION_COLUMN (tok_range.m_finish));
}

/* Use assert_token_loc_eq to verify the TOK->src_loc, using
   SELFTEST_LOCATION as the effective location of the selftest.  */

#define ASSERT_TOKEN_LOC_EQ(TOK, EXP_FILENAME, EXP_LINENUM, \
			    EXP_START_COL, EXP_FINISH_COL) \
  assert_token_loc_eq (SELFTEST_LOCATION, (TOK), (EXP_FILENAME), \
		       (EXP_LINENUM), (EXP_START_COL), (EXP_FINISH_COL))

/* Test of lexing a file using libcpp, verifying tokens and their
   location information.  */

static void
test_lexer (const line_table_case &case_)
{
  /* Create a tempfile and write some text to it.  */
  const char *content =
    /*00000000011111111112222222222333333.3333444444444.455555555556
      12345678901234567890123456789012345.6789012345678.901234567890.  */
    ("test_name /* c-style comment */\n"
     "                                  \"test literal\"\n"
     " // test c++-style comment\n"
     "   42\n");
  temp_source_file tmp (SELFTEST_LOCATION, ".txt", content);

  line_table_test ltt (case_);

  cpp_reader *parser = cpp_create_reader (CLK_GNUC89, NULL, line_table);

  const char *fname = cpp_read_main_file (parser, tmp.get_filename ());
  ASSERT_NE (fname, NULL);

  /* Verify that we get the expected tokens back, with the correct
     location information.  */

  location_t loc;
  const cpp_token *tok;
  tok = cpp_get_token_with_location (parser, &loc);
  ASSERT_NE (tok, NULL);
  ASSERT_EQ (tok->type, CPP_NAME);
  ASSERT_TOKEN_AS_TEXT_EQ (parser, tok, "test_name");
  ASSERT_TOKEN_LOC_EQ (tok, tmp.get_filename (), 1, 1, 9);

  tok = cpp_get_token_with_location (parser, &loc);
  ASSERT_NE (tok, NULL);
  ASSERT_EQ (tok->type, CPP_STRING);
  ASSERT_TOKEN_AS_TEXT_EQ (parser, tok, "\"test literal\"");
  ASSERT_TOKEN_LOC_EQ (tok, tmp.get_filename (), 2, 35, 48);

  tok = cpp_get_token_with_location (parser, &loc);
  ASSERT_NE (tok, NULL);
  ASSERT_EQ (tok->type, CPP_NUMBER);
  ASSERT_TOKEN_AS_TEXT_EQ (parser, tok, "42");
  ASSERT_TOKEN_LOC_EQ (tok, tmp.get_filename (), 4, 4, 5);

  tok = cpp_get_token_with_location (parser, &loc);
  ASSERT_NE (tok, NULL);
  ASSERT_EQ (tok->type, CPP_EOF);

  cpp_finish (parser, NULL);
  cpp_destroy (parser);
}

/* Forward decls.  */

class lexer_test;
class lexer_test_options;

/* A class for specifying options of a lexer_test.
   The "apply" vfunc is called during the lexer_test constructor.  */

class lexer_test_options
{
 public:
  virtual void apply (lexer_test &) = 0;
};

/* Wrapper around an cpp_reader *, which calls cpp_finish and cpp_destroy
   in its dtor.

   This is needed by struct lexer_test to ensure that the cleanup of the
   cpp_reader happens *after* the cleanup of the temp_source_file.  */

class cpp_reader_ptr
{
 public:
  cpp_reader_ptr (cpp_reader *ptr) : m_ptr (ptr) {}

  ~cpp_reader_ptr ()
  {
    cpp_finish (m_ptr, NULL);
    cpp_destroy (m_ptr);
  }

  operator cpp_reader * () const { return m_ptr; }

 private:
  cpp_reader *m_ptr;
};

/* A struct for writing lexer tests.  */

class lexer_test
{
public:
  lexer_test (const line_table_case &case_, const char *content,
	      lexer_test_options *options);
  ~lexer_test ();

  const cpp_token *get_token ();

  /* The ordering of these fields matters.
     The line_table_test must be first, since the cpp_reader_ptr
     uses it.
     The cpp_reader must be cleaned up *after* the temp_source_file
     since the filenames in input.cc's input cache are owned by the
     cpp_reader; in particular, when ~temp_source_file evicts the
     filename the filenames must still be alive.  */
  line_table_test m_ltt;
  cpp_reader_ptr m_parser;
  temp_source_file m_tempfile;
  file_cache m_file_cache;
  string_concat_db m_concats;
  bool m_implicitly_expect_EOF;
};

/* Use an EBCDIC encoding for the execution charset, specifically
   IBM1047-encoded (aka "EBCDIC 1047", or "Code page 1047").

   This exercises iconv integration within libcpp.
   Not every build of iconv supports the given charset,
   so we need to flag this error and handle it gracefully.  */

class ebcdic_execution_charset : public lexer_test_options
{
 public:
  ebcdic_execution_charset () : m_num_iconv_errors (0)
    {
      gcc_assert (s_singleton == NULL);
      s_singleton = this;
    }
  ~ebcdic_execution_charset ()
    {
      gcc_assert (s_singleton == this);
      s_singleton = NULL;
    }

  void apply (lexer_test &test) final override
  {
    cpp_options *cpp_opts = cpp_get_options (test.m_parser);
    cpp_opts->narrow_charset = "IBM1047";

    cpp_callbacks *callbacks = cpp_get_callbacks (test.m_parser);
    callbacks->diagnostic = on_diagnostic;
  }

  static bool on_diagnostic (cpp_reader *pfile ATTRIBUTE_UNUSED,
			     enum cpp_diagnostic_level level ATTRIBUTE_UNUSED,
			     enum cpp_warning_reason reason ATTRIBUTE_UNUSED,
			     rich_location *richloc ATTRIBUTE_UNUSED,
			     const char *msgid, va_list *ap ATTRIBUTE_UNUSED)
    ATTRIBUTE_FPTR_PRINTF(5,0)
  {
    gcc_assert (s_singleton);
    /* Avoid exgettext from picking this up, it is translated in libcpp.  */
    const char *msg = "conversion from %s to %s not supported by iconv";
#ifdef ENABLE_NLS
    msg = dgettext ("cpplib", msg);
#endif
    /* Detect and record errors emitted by libcpp/charset.cc:init_iconv_desc
       when the local iconv build doesn't support the conversion.  */
    if (strcmp (msgid, msg) == 0)
      {
	s_singleton->m_num_iconv_errors++;
	return true;
      }

    /* Otherwise, we have an unexpected error.  */
    abort ();
  }

  bool iconv_errors_occurred_p () const { return m_num_iconv_errors > 0; }

 private:
  static ebcdic_execution_charset *s_singleton;
  int m_num_iconv_errors;
};

ebcdic_execution_charset *ebcdic_execution_charset::s_singleton;

/* A lexer_test_options subclass that records a list of diagnostic
   messages emitted by the lexer.  */

class lexer_diagnostic_sink : public lexer_test_options
{
 public:
  lexer_diagnostic_sink ()
  {
    gcc_assert (s_singleton == NULL);
    s_singleton = this;
  }
  ~lexer_diagnostic_sink ()
  {
    gcc_assert (s_singleton == this);
    s_singleton = NULL;

    int i;
    char *str;
    FOR_EACH_VEC_ELT (m_diagnostics, i, str)
      free (str);
  }

  void apply (lexer_test &test) final override
  {
    cpp_callbacks *callbacks = cpp_get_callbacks (test.m_parser);
    callbacks->diagnostic = on_diagnostic;
  }

  static bool on_diagnostic (cpp_reader *pfile ATTRIBUTE_UNUSED,
			     enum cpp_diagnostic_level level ATTRIBUTE_UNUSED,
			     enum cpp_warning_reason reason ATTRIBUTE_UNUSED,
			     rich_location *richloc ATTRIBUTE_UNUSED,
			     const char *msgid, va_list *ap)
    ATTRIBUTE_FPTR_PRINTF(5,0)
  {
    char *msg = xvasprintf (msgid, *ap);
    s_singleton->m_diagnostics.safe_push (msg);
    return true;
  }

  auto_vec<char *> m_diagnostics;

 private:
  static lexer_diagnostic_sink *s_singleton;
};

lexer_diagnostic_sink *lexer_diagnostic_sink::s_singleton;

/* Constructor.  Override line_table with a new instance based on CASE_,
   and write CONTENT to a tempfile.  Create a cpp_reader, and use it to
   start parsing the tempfile.  */

lexer_test::lexer_test (const line_table_case &case_, const char *content,
			lexer_test_options *options)
: m_ltt (case_),
  m_parser (cpp_create_reader (CLK_GNUC99, NULL, line_table)),
  /* Create a tempfile and write the text to it.  */
  m_tempfile (SELFTEST_LOCATION, ".c", content),
  m_concats (),
  m_implicitly_expect_EOF (true)
{
  if (options)
    options->apply (*this);

  cpp_init_iconv (m_parser);

  /* Parse the file.  */
  const char *fname = cpp_read_main_file (m_parser,
					  m_tempfile.get_filename ());
  ASSERT_NE (fname, NULL);
}

/* Destructor.  By default, verify that the next token in m_parser is EOF.  */

lexer_test::~lexer_test ()
{
  location_t loc;
  const cpp_token *tok;

  if (m_implicitly_expect_EOF)
    {
      tok = cpp_get_token_with_location (m_parser, &loc);
      ASSERT_NE (tok, NULL);
      ASSERT_EQ (tok->type, CPP_EOF);
    }
}

/* Get the next token from m_parser.  */

const cpp_token *
lexer_test::get_token ()
{
  location_t loc;
  const cpp_token *tok;

  tok = cpp_get_token_with_location (m_parser, &loc);
  ASSERT_NE (tok, NULL);
  return tok;
}

/* Verify that locations within string literals are correctly handled.  */

/* Verify get_source_range_for_substring for token(s) at STRLOC,
   using the string concatenation database for TEST.

   Assert that the character at index IDX is on EXPECTED_LINE,
   and that it begins at column EXPECTED_START_COL and ends at
   EXPECTED_FINISH_COL (unless the locations are beyond
   LINE_MAP_MAX_LOCATION_WITH_COLS, in which case don't check their
   columns).  */

static void
assert_char_at_range (const location &loc,
		      lexer_test& test,
		      location_t strloc, enum cpp_ttype type, int idx,
		      int expected_line, int expected_start_col,
		      int expected_finish_col)
{
  cpp_reader *pfile = test.m_parser;
  string_concat_db *concats = &test.m_concats;

  source_range actual_range = source_range();
  const char *err
    = get_source_range_for_char (pfile, test.m_file_cache,
				 concats, strloc, type, idx,
				 &actual_range);
  if (should_have_column_data_p (strloc))
    ASSERT_EQ_AT (loc, NULL, err);
  else
    {
      ASSERT_STREQ_AT (loc,
		       "range starts after LINE_MAP_MAX_LOCATION_WITH_COLS",
		       err);
      return;
    }

  int actual_start_line = LOCATION_LINE (actual_range.m_start);
  ASSERT_EQ_AT (loc, expected_line, actual_start_line);
  int actual_finish_line = LOCATION_LINE (actual_range.m_finish);
  ASSERT_EQ_AT (loc, expected_line, actual_finish_line);

  if (should_have_column_data_p (actual_range.m_start))
    {
      int actual_start_col = LOCATION_COLUMN (actual_range.m_start);
      ASSERT_EQ_AT (loc, expected_start_col, actual_start_col);
    }
  if (should_have_column_data_p (actual_range.m_finish))
    {
      int actual_finish_col = LOCATION_COLUMN (actual_range.m_finish);
      ASSERT_EQ_AT (loc, expected_finish_col, actual_finish_col);
    }
}

/* Macro for calling assert_char_at_range, supplying SELFTEST_LOCATION for
   the effective location of any errors.  */

#define ASSERT_CHAR_AT_RANGE(LEXER_TEST, STRLOC, TYPE, IDX, EXPECTED_LINE, \
			     EXPECTED_START_COL, EXPECTED_FINISH_COL)	\
  assert_char_at_range (SELFTEST_LOCATION, (LEXER_TEST), (STRLOC), (TYPE), \
			(IDX), (EXPECTED_LINE), (EXPECTED_START_COL), \
			(EXPECTED_FINISH_COL))

/* Verify get_num_source_ranges_for_substring for token(s) at STRLOC,
   using the string concatenation database for TEST.

   Assert that the token(s) at STRLOC contain EXPECTED_NUM_RANGES.  */

static void
assert_num_substring_ranges (const location &loc,
			     lexer_test& test,
			     location_t strloc,
			     enum cpp_ttype type,
			     int expected_num_ranges)
{
  cpp_reader *pfile = test.m_parser;
  string_concat_db *concats = &test.m_concats;

  int actual_num_ranges = -1;
  const char *err
    = get_num_source_ranges_for_substring (pfile, test.m_file_cache,
					   concats, strloc, type,
					   &actual_num_ranges);
  if (should_have_column_data_p (strloc))
    ASSERT_EQ_AT (loc, NULL, err);
  else
    {
      ASSERT_STREQ_AT (loc,
		       "range starts after LINE_MAP_MAX_LOCATION_WITH_COLS",
		       err);
      return;
    }
  ASSERT_EQ_AT (loc, expected_num_ranges, actual_num_ranges);
}

/* Macro for calling assert_num_substring_ranges, supplying
   SELFTEST_LOCATION for the effective location of any errors.  */

#define ASSERT_NUM_SUBSTRING_RANGES(LEXER_TEST, STRLOC, TYPE, \
				    EXPECTED_NUM_RANGES)		\
  assert_num_substring_ranges (SELFTEST_LOCATION, (LEXER_TEST), (STRLOC), \
			       (TYPE), (EXPECTED_NUM_RANGES))


/* Verify that get_num_source_ranges_for_substring for token(s) at STRLOC
   returns an error (using the string concatenation database for TEST).  */

static void
assert_has_no_substring_ranges (const location &loc,
				lexer_test& test,
				location_t strloc,
				enum cpp_ttype type,
				const char *expected_err)
{
  cpp_reader *pfile = test.m_parser;
  string_concat_db *concats = &test.m_concats;
  cpp_substring_ranges ranges;
  const char *actual_err
    = get_substring_ranges_for_loc (pfile, test.m_file_cache, concats, strloc,
				    type, ranges);
  if (should_have_column_data_p (strloc))
    ASSERT_STREQ_AT (loc, expected_err, actual_err);
  else
    ASSERT_STREQ_AT (loc,
		     "range starts after LINE_MAP_MAX_LOCATION_WITH_COLS",
		     actual_err);
}

#define ASSERT_HAS_NO_SUBSTRING_RANGES(LEXER_TEST, STRLOC, TYPE, ERR)    \
    assert_has_no_substring_ranges (SELFTEST_LOCATION, (LEXER_TEST), \
				    (STRLOC), (TYPE), (ERR))

/* Lex a simple string literal.  Verify the substring location data, before
   and after running cpp_interpret_string on it.  */

static void
test_lexer_string_locations_simple (const line_table_case &case_)
{
  /* Digits 0-9 (with 0 at column 10), the simple way.
     ....................000000000.11111111112.2222222223333333333
     ....................123456789.01234567890.1234567890123456789
     We add a trailing comment to ensure that we correctly locate
     the end of the string literal token.  */
  const char *content = "        \"0123456789\" /* not a string */\n";
  lexer_test test (case_, content, NULL);

  /* Verify that we get the expected token back, with the correct
     location information.  */
  const cpp_token *tok = test.get_token ();
  ASSERT_EQ (tok->type, CPP_STRING);
  ASSERT_TOKEN_AS_TEXT_EQ (test.m_parser, tok, "\"0123456789\"");
  ASSERT_TOKEN_LOC_EQ (tok, test.m_tempfile.get_filename (), 1, 9, 20);

  /* At this point in lexing, the quote characters are treated as part of
     the string (they are stripped off by cpp_interpret_string).  */

  ASSERT_EQ (tok->val.str.len, 12);

  /* Verify that cpp_interpret_string works.  */
  cpp_string dst_string;
  const enum cpp_ttype type = CPP_STRING;
  bool result = cpp_interpret_string (test.m_parser, &tok->val.str, 1,
				      &dst_string, type);
  ASSERT_TRUE (result);
  ASSERT_STREQ ("0123456789", (const char *)dst_string.text);
  free (const_cast <unsigned char *> (dst_string.text));

  /* Verify ranges of individual characters.  This no longer includes the
     opening quote, but does include the closing quote.  */
  for (int i = 0; i <= 10; i++)
    ASSERT_CHAR_AT_RANGE (test, tok->src_loc, type, i, 1,
			  10 + i, 10 + i);

  ASSERT_NUM_SUBSTRING_RANGES (test, tok->src_loc, type, 11);
}

/* As test_lexer_string_locations_simple, but use an EBCDIC execution
   encoding.  */

static void
test_lexer_string_locations_ebcdic (const line_table_case &case_)
{
  /* EBCDIC support requires iconv.  */
  if (!HAVE_ICONV)
    return;

  /* Digits 0-9 (with 0 at column 10), the simple way.
     ....................000000000.11111111112.2222222223333333333
     ....................123456789.01234567890.1234567890123456789
     We add a trailing comment to ensure that we correctly locate
     the end of the string literal token.  */
  const char *content = "        \"0123456789\" /* not a string */\n";
  ebcdic_execution_charset use_ebcdic;
  lexer_test test (case_, content, &use_ebcdic);

  /* Verify that we get the expected token back, with the correct
     location information.  */
  const cpp_token *tok = test.get_token ();
  ASSERT_EQ (tok->type, CPP_STRING);
  ASSERT_TOKEN_AS_TEXT_EQ (test.m_parser, tok, "\"0123456789\"");
  ASSERT_TOKEN_LOC_EQ (tok, test.m_tempfile.get_filename (), 1, 9, 20);

  /* At this point in lexing, the quote characters are treated as part of
     the string (they are stripped off by cpp_interpret_string).  */

  ASSERT_EQ (tok->val.str.len, 12);

  /* The remainder of the test requires an iconv implementation that
     can convert from UTF-8 to the EBCDIC encoding requested above.  */
  if (use_ebcdic.iconv_errors_occurred_p ())
    return;

  /* Verify that cpp_interpret_string works.  */
  cpp_string dst_string;
  const enum cpp_ttype type = CPP_STRING;
  bool result = cpp_interpret_string (test.m_parser, &tok->val.str, 1,
				      &dst_string, type);
  ASSERT_TRUE (result);
  /* We should now have EBCDIC-encoded text, specifically
     IBM1047-encoded (aka "EBCDIC 1047", or "Code page 1047").
     The digits 0-9 are encoded as 240-249 i.e. 0xf0-0xf9.  */
  ASSERT_STREQ ("\xf0\xf1\xf2\xf3\xf4\xf5\xf6\xf7\xf8\xf9",
		(const char *)dst_string.text);
  free (const_cast <unsigned char *> (dst_string.text));

  /* Verify that we don't attempt to record substring location information
     for such cases.  */
  ASSERT_HAS_NO_SUBSTRING_RANGES
    (test, tok->src_loc, type,
     "execution character set != source character set");
}

/* Lex a string literal containing a hex-escaped character.
   Verify the substring location data, before and after running
   cpp_interpret_string on it.  */

static void
test_lexer_string_locations_hex (const line_table_case &case_)
{
  /* Digits 0-9, expressing digit 5 in ASCII as "\x35"
     and with a space in place of digit 6, to terminate the escaped
     hex code.
     ....................000000000.111111.11112222.
     ....................123456789.012345.67890123.  */
  const char *content = "        \"01234\\x35 789\"\n";
  lexer_test test (case_, content, NULL);

  /* Verify that we get the expected token back, with the correct
     location information.  */
  const cpp_token *tok = test.get_token ();
  ASSERT_EQ (tok->type, CPP_STRING);
  ASSERT_TOKEN_AS_TEXT_EQ (test.m_parser, tok, "\"01234\\x35 789\"");
  ASSERT_TOKEN_LOC_EQ (tok, test.m_tempfile.get_filename (), 1, 9, 23);

  /* At this point in lexing, the quote characters are treated as part of
     the string (they are stripped off by cpp_interpret_string).  */
  ASSERT_EQ (tok->val.str.len, 15);

  /* Verify that cpp_interpret_string works.  */
  cpp_string dst_string;
  const enum cpp_ttype type = CPP_STRING;
  bool result = cpp_interpret_string (test.m_parser, &tok->val.str, 1,
				      &dst_string, type);
  ASSERT_TRUE (result);
  ASSERT_STREQ ("012345 789", (const char *)dst_string.text);
  free (const_cast <unsigned char *> (dst_string.text));

  /* Verify ranges of individual characters.  This no longer includes the
     opening quote, but does include the closing quote.  */
  for (int i = 0; i <= 4; i++)
    ASSERT_CHAR_AT_RANGE (test, tok->src_loc, type, i, 1, 10 + i, 10 + i);
  ASSERT_CHAR_AT_RANGE (test, tok->src_loc, type, 5, 1, 15, 18);
  for (int i = 6; i <= 10; i++)
    ASSERT_CHAR_AT_RANGE (test, tok->src_loc, type, i, 1, 13 + i, 13 + i);

  ASSERT_NUM_SUBSTRING_RANGES (test, tok->src_loc, type, 11);
}

/* Lex a string literal containing an octal-escaped character.
   Verify the substring location data after running cpp_interpret_string
   on it.  */

static void
test_lexer_string_locations_oct (const line_table_case &case_)
{
  /* Digits 0-9, expressing digit 5 in ASCII as "\065"
     and with a space in place of digit 6, to terminate the escaped
     octal code.
     ....................000000000.111111.11112222.2222223333333333444
     ....................123456789.012345.67890123.4567890123456789012  */
  const char *content = "        \"01234\\065 789\" /* not a string */\n";
  lexer_test test (case_, content, NULL);

  /* Verify that we get the expected token back, with the correct
     location information.  */
  const cpp_token *tok = test.get_token ();
  ASSERT_EQ (tok->type, CPP_STRING);
  ASSERT_TOKEN_AS_TEXT_EQ (test.m_parser, tok, "\"01234\\065 789\"");

  /* Verify that cpp_interpret_string works.  */
  cpp_string dst_string;
  const enum cpp_ttype type = CPP_STRING;
  bool result = cpp_interpret_string (test.m_parser, &tok->val.str, 1,
				      &dst_string, type);
  ASSERT_TRUE (result);
  ASSERT_STREQ ("012345 789", (const char *)dst_string.text);
  free (const_cast <unsigned char *> (dst_string.text));

  /* Verify ranges of individual characters.  This no longer includes the
     opening quote, but does include the closing quote.  */
  for (int i = 0; i < 5; i++)
    ASSERT_CHAR_AT_RANGE (test, tok->src_loc, type, i, 1, 10 + i, 10 + i);
  ASSERT_CHAR_AT_RANGE (test, tok->src_loc, type, 5, 1, 15, 18);
  for (int i = 6; i <= 10; i++)
    ASSERT_CHAR_AT_RANGE (test, tok->src_loc, type, i, 1, 13 + i, 13 + i);

  ASSERT_NUM_SUBSTRING_RANGES (test, tok->src_loc, type, 11);
}

/* Test of string literal containing letter escapes.  */

static void
test_lexer_string_locations_letter_escape_1 (const line_table_case &case_)
{
  /* The string "\tfoo\\\nbar" i.e. tab, "foo", backslash, newline, bar.
     .....................000000000.1.11111.1.1.11222.22222223333333
     .....................123456789.0.12345.6.7.89012.34567890123456.  */
  const char *content = ("        \"\\tfoo\\\\\\nbar\" /* non-str */\n");
  lexer_test test (case_, content, NULL);

  /* Verify that we get the expected tokens back.  */
  const cpp_token *tok = test.get_token ();
  ASSERT_EQ (tok->type, CPP_STRING);
  ASSERT_TOKEN_AS_TEXT_EQ (test.m_parser, tok, "\"\\tfoo\\\\\\nbar\"");

  /* Verify ranges of individual characters. */
  /* "\t".  */
  ASSERT_CHAR_AT_RANGE (test, tok->src_loc, CPP_STRING,
			0, 1, 10, 11);
  /* "foo". */
  for (int i = 1; i <= 3; i++)
    ASSERT_CHAR_AT_RANGE (test, tok->src_loc, CPP_STRING,
			  i, 1, 11 + i, 11 + i);
  /* "\\" and "\n".  */
  ASSERT_CHAR_AT_RANGE (test, tok->src_loc, CPP_STRING,
			4, 1, 15, 16);
  ASSERT_CHAR_AT_RANGE (test, tok->src_loc, CPP_STRING,
			5, 1, 17, 18);

  /* "bar" and closing quote for nul-terminator.  */
  for (int i = 6; i <= 9; i++)
    ASSERT_CHAR_AT_RANGE (test, tok->src_loc, CPP_STRING,
			  i, 1, 13 + i, 13 + i);

  ASSERT_NUM_SUBSTRING_RANGES (test, tok->src_loc, CPP_STRING, 10);
}

/* Another test of a string literal containing a letter escape.
   Based on string seen in
     printf ("%-%\n");
   in gcc.dg/format/c90-printf-1.c.  */

static void
test_lexer_string_locations_letter_escape_2 (const line_table_case &case_)
{
  /* .....................000000000.1111.11.1111.22222222223.
     .....................123456789.0123.45.6789.01234567890.  */
  const char *content = ("        \"%-%\\n\" /* non-str */\n");
  lexer_test test (case_, content, NULL);

  /* Verify that we get the expected tokens back.  */
  const cpp_token *tok = test.get_token ();
  ASSERT_EQ (tok->type, CPP_STRING);
  ASSERT_TOKEN_AS_TEXT_EQ (test.m_parser, tok, "\"%-%\\n\"");

  /* Verify ranges of individual characters. */
  /* "%-%".  */
  for (int i = 0; i < 3; i++)
    ASSERT_CHAR_AT_RANGE (test, tok->src_loc, CPP_STRING,
			  i, 1, 10 + i, 10 + i);
  /* "\n".  */
  ASSERT_CHAR_AT_RANGE (test, tok->src_loc, CPP_STRING,
			3, 1, 13, 14);

  /* Closing quote for nul-terminator.  */
  ASSERT_CHAR_AT_RANGE (test, tok->src_loc, CPP_STRING,
			4, 1, 15, 15);

  ASSERT_NUM_SUBSTRING_RANGES (test, tok->src_loc, CPP_STRING, 5);
}

/* Lex a string literal containing UCN 4 characters.
   Verify the substring location data after running cpp_interpret_string
   on it.  */

static void
test_lexer_string_locations_ucn4 (const line_table_case &case_)
{
  /* Digits 0-9, expressing digits 5 and 6 as Roman numerals expressed
     as UCN 4.
     ....................000000000.111111.111122.222222223.33333333344444
     ....................123456789.012345.678901.234567890.12345678901234  */
  const char *content = "        \"01234\\u2174\\u2175789\" /* non-str */\n";
  lexer_test test (case_, content, NULL);

  /* Verify that we get the expected token back, with the correct
     location information.  */
  const cpp_token *tok = test.get_token ();
  ASSERT_EQ (tok->type, CPP_STRING);
  ASSERT_TOKEN_AS_TEXT_EQ (test.m_parser, tok, "\"01234\\u2174\\u2175789\"");

  /* Verify that cpp_interpret_string works.
     The string should be encoded in the execution character
     set.  Assuming that is UTF-8, we should have the following:
     -----------  ----  -----  -------  ----------------
     Byte offset  Byte  Octal  Unicode  Source Column(s)
     -----------  ----  -----  -------  ----------------
     0            0x30         '0'      10
     1            0x31         '1'      11
     2            0x32         '2'      12
     3            0x33         '3'      13
     4            0x34         '4'      14
     5            0xE2  \342   U+2174   15-20
     6            0x85  \205    (cont)  15-20
     7            0xB4  \264    (cont)  15-20
     8            0xE2  \342   U+2175   21-26
     9            0x85  \205    (cont)  21-26
     10           0xB5  \265    (cont)  21-26
     11           0x37         '7'      27
     12           0x38         '8'      28
     13           0x39         '9'      29
     14           0x00                  30 (closing quote)
     -----------  ----  -----  -------  ---------------.  */

  cpp_string dst_string;
  const enum cpp_ttype type = CPP_STRING;
  bool result = cpp_interpret_string (test.m_parser, &tok->val.str, 1,
				      &dst_string, type);
  ASSERT_TRUE (result);
  ASSERT_STREQ ("01234\342\205\264\342\205\265789",
		(const char *)dst_string.text);
  free (const_cast <unsigned char *> (dst_string.text));

  /* Verify ranges of individual characters.  This no longer includes the
     opening quote, but does include the closing quote.
     '01234'.  */
  for (int i = 0; i <= 4; i++)
    ASSERT_CHAR_AT_RANGE (test, tok->src_loc, type, i, 1, 10 + i, 10 + i);
  /* U+2174.  */
  for (int i = 5; i <= 7; i++)
    ASSERT_CHAR_AT_RANGE (test, tok->src_loc, type, i, 1, 15, 20);
  /* U+2175.  */
  for (int i = 8; i <= 10; i++)
    ASSERT_CHAR_AT_RANGE (test, tok->src_loc, type, i, 1, 21, 26);
  /* '789' and nul terminator  */
  for (int i = 11; i <= 14; i++)
    ASSERT_CHAR_AT_RANGE (test, tok->src_loc, type, i, 1, 16 + i, 16 + i);

  ASSERT_NUM_SUBSTRING_RANGES (test, tok->src_loc, type, 15);
}

/* Lex a string literal containing UCN 8 characters.
   Verify the substring location data after running cpp_interpret_string
   on it.  */

static void
test_lexer_string_locations_ucn8 (const line_table_case &case_)
{
  /* Digits 0-9, expressing digits 5 and 6 as Roman numerals as UCN 8.
     ....................000000000.111111.1111222222.2222333333333.344444
     ....................123456789.012345.6789012345.6789012345678.901234  */
  const char *content = "        \"01234\\U00002174\\U00002175789\" /* */\n";
  lexer_test test (case_, content, NULL);

  /* Verify that we get the expected token back, with the correct
     location information.  */
  const cpp_token *tok = test.get_token ();
  ASSERT_EQ (tok->type, CPP_STRING);
  ASSERT_TOKEN_AS_TEXT_EQ (test.m_parser, tok,
			   "\"01234\\U00002174\\U00002175789\"");

  /* Verify that cpp_interpret_string works.
     The UTF-8 encoding of the string is identical to that from
     the ucn4 testcase above; the only difference is the column
     locations.  */
  cpp_string dst_string;
  const enum cpp_ttype type = CPP_STRING;
  bool result = cpp_interpret_string (test.m_parser, &tok->val.str, 1,
				      &dst_string, type);
  ASSERT_TRUE (result);
  ASSERT_STREQ ("01234\342\205\264\342\205\265789",
		(const char *)dst_string.text);
  free (const_cast <unsigned char *> (dst_string.text));

  /* Verify ranges of individual characters.  This no longer includes the
     opening quote, but does include the closing quote.
     '01234'.  */
  for (int i = 0; i <= 4; i++)
    ASSERT_CHAR_AT_RANGE (test, tok->src_loc, type, i, 1, 10 + i, 10 + i);
  /* U+2174.  */
  for (int i = 5; i <= 7; i++)
    ASSERT_CHAR_AT_RANGE (test, tok->src_loc, type, i, 1, 15, 24);
  /* U+2175.  */
  for (int i = 8; i <= 10; i++)
    ASSERT_CHAR_AT_RANGE (test, tok->src_loc, type, i, 1, 25, 34);
  /* '789' at columns 35-37  */
  for (int i = 11; i <= 13; i++)
    ASSERT_CHAR_AT_RANGE (test, tok->src_loc, type, i, 1, 24 + i, 24 + i);
  /* Closing quote/nul-terminator at column 38.  */
  ASSERT_CHAR_AT_RANGE (test, tok->src_loc, type, 14, 1, 38, 38);

  ASSERT_NUM_SUBSTRING_RANGES (test, tok->src_loc, type, 15);
}

/* Fetch a big-endian 32-bit value and convert to host endianness.  */

static uint32_t
uint32_from_big_endian (const uint32_t *ptr_be_value)
{
  const unsigned char *buf = (const unsigned char *)ptr_be_value;
  return (((uint32_t) buf[0] << 24)
	  | ((uint32_t) buf[1] << 16)
	  | ((uint32_t) buf[2] << 8)
	  | (uint32_t) buf[3]);
}

/* Lex a wide string literal and verify that attempts to read substring
   location data from it fail gracefully.  */

static void
test_lexer_string_locations_wide_string (const line_table_case &case_)
{
  /* Digits 0-9.
     ....................000000000.11111111112.22222222233333
     ....................123456789.01234567890.12345678901234  */
  const char *content = "       L\"0123456789\" /* non-str */\n";
  lexer_test test (case_, content, NULL);

  /* Verify that we get the expected token back, with the correct
     location information.  */
  const cpp_token *tok = test.get_token ();
  ASSERT_EQ (tok->type, CPP_WSTRING);
  ASSERT_TOKEN_AS_TEXT_EQ (test.m_parser, tok, "L\"0123456789\"");

  /* Verify that cpp_interpret_string works, using CPP_WSTRING.  */
  cpp_string dst_string;
  const enum cpp_ttype type = CPP_WSTRING;
  bool result = cpp_interpret_string (test.m_parser, &tok->val.str, 1,
				      &dst_string, type);
  ASSERT_TRUE (result);
  /* The cpp_reader defaults to big-endian with
     CHAR_BIT * sizeof (int) for the wchar_precision, so dst_string should
     now be encoded as UTF-32BE.  */
  const uint32_t *be32_chars = (const uint32_t *)dst_string.text;
  ASSERT_EQ ('0', uint32_from_big_endian (&be32_chars[0]));
  ASSERT_EQ ('5', uint32_from_big_endian (&be32_chars[5]));
  ASSERT_EQ ('9', uint32_from_big_endian (&be32_chars[9]));
  ASSERT_EQ (0, uint32_from_big_endian (&be32_chars[10]));
  free (const_cast <unsigned char *> (dst_string.text));

  /* We don't yet support generating substring location information
     for L"" strings.  */
  ASSERT_HAS_NO_SUBSTRING_RANGES
    (test, tok->src_loc, type,
     "execution character set != source character set");
}

/* Fetch a big-endian 16-bit value and convert to host endianness.  */

static uint16_t
uint16_from_big_endian (const uint16_t *ptr_be_value)
{
  const unsigned char *buf = (const unsigned char *)ptr_be_value;
  return ((uint16_t) buf[0] << 8) | (uint16_t) buf[1];
}

/* Lex a u"" string literal and verify that attempts to read substring
   location data from it fail gracefully.  */

static void
test_lexer_string_locations_string16 (const line_table_case &case_)
{
  /* Digits 0-9.
     ....................000000000.11111111112.22222222233333
     ....................123456789.01234567890.12345678901234  */
  const char *content = "       u\"0123456789\" /* non-str */\n";
  lexer_test test (case_, content, NULL);

  /* Verify that we get the expected token back, with the correct
     location information.  */
  const cpp_token *tok = test.get_token ();
  ASSERT_EQ (tok->type, CPP_STRING16);
  ASSERT_TOKEN_AS_TEXT_EQ (test.m_parser, tok, "u\"0123456789\"");

  /* Verify that cpp_interpret_string works, using CPP_STRING16.  */
  cpp_string dst_string;
  const enum cpp_ttype type = CPP_STRING16;
  bool result = cpp_interpret_string (test.m_parser, &tok->val.str, 1,
				      &dst_string, type);
  ASSERT_TRUE (result);

  /* The cpp_reader defaults to big-endian, so dst_string should
     now be encoded as UTF-16BE.  */
  const uint16_t *be16_chars = (const uint16_t *)dst_string.text;
  ASSERT_EQ ('0', uint16_from_big_endian (&be16_chars[0]));
  ASSERT_EQ ('5', uint16_from_big_endian (&be16_chars[5]));
  ASSERT_EQ ('9', uint16_from_big_endian (&be16_chars[9]));
  ASSERT_EQ (0, uint16_from_big_endian (&be16_chars[10]));
  free (const_cast <unsigned char *> (dst_string.text));

  /* We don't yet support generating substring location information
     for L"" strings.  */
  ASSERT_HAS_NO_SUBSTRING_RANGES
    (test, tok->src_loc, type,
     "execution character set != source character set");
}

/* Lex a U"" string literal and verify that attempts to read substring
   location data from it fail gracefully.  */

static void
test_lexer_string_locations_string32 (const line_table_case &case_)
{
  /* Digits 0-9.
     ....................000000000.11111111112.22222222233333
     ....................123456789.01234567890.12345678901234  */
  const char *content = "       U\"0123456789\" /* non-str */\n";
  lexer_test test (case_, content, NULL);

  /* Verify that we get the expected token back, with the correct
     location information.  */
  const cpp_token *tok = test.get_token ();
  ASSERT_EQ (tok->type, CPP_STRING32);
  ASSERT_TOKEN_AS_TEXT_EQ (test.m_parser, tok, "U\"0123456789\"");

  /* Verify that cpp_interpret_string works, using CPP_STRING32.  */
  cpp_string dst_string;
  const enum cpp_ttype type = CPP_STRING32;
  bool result = cpp_interpret_string (test.m_parser, &tok->val.str, 1,
				      &dst_string, type);
  ASSERT_TRUE (result);

  /* The cpp_reader defaults to big-endian, so dst_string should
     now be encoded as UTF-32BE.  */
  const uint32_t *be32_chars = (const uint32_t *)dst_string.text;
  ASSERT_EQ ('0', uint32_from_big_endian (&be32_chars[0]));
  ASSERT_EQ ('5', uint32_from_big_endian (&be32_chars[5]));
  ASSERT_EQ ('9', uint32_from_big_endian (&be32_chars[9]));
  ASSERT_EQ (0, uint32_from_big_endian (&be32_chars[10]));
  free (const_cast <unsigned char *> (dst_string.text));

  /* We don't yet support generating substring location information
     for L"" strings.  */
  ASSERT_HAS_NO_SUBSTRING_RANGES
    (test, tok->src_loc, type,
     "execution character set != source character set");
}

/* Lex a u8-string literal.
   Verify the substring location data after running cpp_interpret_string
   on it.  */

static void
test_lexer_string_locations_u8 (const line_table_case &case_)
{
  /* Digits 0-9.
     ....................000000000.11111111112.22222222233333
     ....................123456789.01234567890.12345678901234  */
  const char *content = "      u8\"0123456789\" /* non-str */\n";
  lexer_test test (case_, content, NULL);

  /* Verify that we get the expected token back, with the correct
     location information.  */
  const cpp_token *tok = test.get_token ();
  ASSERT_EQ (tok->type, CPP_UTF8STRING);
  ASSERT_TOKEN_AS_TEXT_EQ (test.m_parser, tok, "u8\"0123456789\"");

  /* Verify that cpp_interpret_string works.  */
  cpp_string dst_string;
  const enum cpp_ttype type = CPP_STRING;
  bool result = cpp_interpret_string (test.m_parser, &tok->val.str, 1,
				      &dst_string, type);
  ASSERT_TRUE (result);
  ASSERT_STREQ ("0123456789", (const char *)dst_string.text);
  free (const_cast <unsigned char *> (dst_string.text));

  /* Verify ranges of individual characters.  This no longer includes the
     opening quote, but does include the closing quote.  */
  for (int i = 0; i <= 10; i++)
    ASSERT_CHAR_AT_RANGE (test, tok->src_loc, type, i, 1, 10 + i, 10 + i);
}

/* Lex a string literal containing UTF-8 source characters.
   Verify the substring location data after running cpp_interpret_string
   on it.  */

static void
test_lexer_string_locations_utf8_source (const line_table_case &case_)
{
 /* This string literal is written out to the source file as UTF-8,
    and is of the form "before mojibake after", where "mojibake"
    is written as the following four unicode code points:
       U+6587 CJK UNIFIED IDEOGRAPH-6587
       U+5B57 CJK UNIFIED IDEOGRAPH-5B57
       U+5316 CJK UNIFIED IDEOGRAPH-5316
       U+3051 HIRAGANA LETTER KE.
     Each of these is 3 bytes wide when encoded in UTF-8, whereas the
     "before" and "after" are 1 byte per unicode character.

     The numbering shown are "columns", which are *byte* numbers within
     the line, rather than unicode character numbers.

     .................... 000000000.1111111.
     .................... 123456789.0123456.  */
  const char *content = ("        \"before "
			 /* U+6587 CJK UNIFIED IDEOGRAPH-6587
			      UTF-8: 0xE6 0x96 0x87
			      C octal escaped UTF-8: \346\226\207
			    "column" numbers: 17-19.  */
			 "\346\226\207"

			 /* U+5B57 CJK UNIFIED IDEOGRAPH-5B57
			      UTF-8: 0xE5 0xAD 0x97
			      C octal escaped UTF-8: \345\255\227
			    "column" numbers: 20-22.  */
			 "\345\255\227"

			 /* U+5316 CJK UNIFIED IDEOGRAPH-5316
			      UTF-8: 0xE5 0x8C 0x96
			      C octal escaped UTF-8: \345\214\226
			    "column" numbers: 23-25.  */
			 "\345\214\226"

			 /* U+3051 HIRAGANA LETTER KE
			      UTF-8: 0xE3 0x81 0x91
			      C octal escaped UTF-8: \343\201\221
			    "column" numbers: 26-28.  */
			 "\343\201\221"

			 /* column numbers 29 onwards
			  2333333.33334444444444
			  9012345.67890123456789. */
			 " after\" /* non-str */\n");
  lexer_test test (case_, content, NULL);

  /* Verify that we get the expected token back, with the correct
     location information.  */
  const cpp_token *tok = test.get_token ();
  ASSERT_EQ (tok->type, CPP_STRING);
  ASSERT_TOKEN_AS_TEXT_EQ
    (test.m_parser, tok,
     "\"before \346\226\207\345\255\227\345\214\226\343\201\221 after\"");

  /* Verify that cpp_interpret_string works.  */
  cpp_string dst_string;
  const enum cpp_ttype type = CPP_STRING;
  bool result = cpp_interpret_string (test.m_parser, &tok->val.str, 1,
				      &dst_string, type);
  ASSERT_TRUE (result);
  ASSERT_STREQ
    ("before \346\226\207\345\255\227\345\214\226\343\201\221 after",
     (const char *)dst_string.text);
  free (const_cast <unsigned char *> (dst_string.text));

  /* Verify ranges of individual characters.  This no longer includes the
     opening quote, but does include the closing quote.
     Assuming that both source and execution encodings are UTF-8, we have
     a run of 25 octets in each, plus the NUL terminator.  */
  for (int i = 0; i < 25; i++)
    ASSERT_CHAR_AT_RANGE (test, tok->src_loc, type, i, 1, 10 + i, 10 + i);
  /* NUL-terminator should use the closing quote at column 35.  */
  ASSERT_CHAR_AT_RANGE (test, tok->src_loc, type, 25, 1, 35, 35);

  ASSERT_NUM_SUBSTRING_RANGES (test, tok->src_loc, type, 26);
}

/* Test of string literal concatenation.  */

static void
test_lexer_string_locations_concatenation_1 (const line_table_case &case_)
{
  /* Digits 0-9.
     .....................000000000.111111.11112222222222
     .....................123456789.012345.67890123456789.  */
  const char *content = ("        \"01234\" /* non-str */\n"
			 "        \"56789\" /* non-str */\n");
  lexer_test test (case_, content, NULL);

  location_t input_locs[2];

  /* Verify that we get the expected tokens back.  */
  auto_vec <cpp_string> input_strings;
  const cpp_token *tok_a = test.get_token ();
  ASSERT_EQ (tok_a->type, CPP_STRING);
  ASSERT_TOKEN_AS_TEXT_EQ (test.m_parser, tok_a, "\"01234\"");
  input_strings.safe_push (tok_a->val.str);
  input_locs[0] = tok_a->src_loc;

  const cpp_token *tok_b = test.get_token ();
  ASSERT_EQ (tok_b->type, CPP_STRING);
  ASSERT_TOKEN_AS_TEXT_EQ (test.m_parser, tok_b, "\"56789\"");
  input_strings.safe_push (tok_b->val.str);
  input_locs[1] = tok_b->src_loc;

  /* Verify that cpp_interpret_string works.  */
  cpp_string dst_string;
  const enum cpp_ttype type = CPP_STRING;
  bool result = cpp_interpret_string (test.m_parser,
				      input_strings.address (), 2,
				      &dst_string, type);
  ASSERT_TRUE (result);
  ASSERT_STREQ ("0123456789", (const char *)dst_string.text);
  free (const_cast <unsigned char *> (dst_string.text));

  /* Simulate c-lex.cc's lex_string in order to record concatenation.  */
  test.m_concats.record_string_concatenation (2, input_locs);

  location_t initial_loc = input_locs[0];

  /* "01234" on line 1.  */
  for (int i = 0; i <= 4; i++)
    ASSERT_CHAR_AT_RANGE (test, initial_loc, type, i, 1, 10 + i, 10 + i);
  /* "56789" in line 2, plus its closing quote for the nul terminator.  */
  for (int i = 5; i <= 10; i++)
    ASSERT_CHAR_AT_RANGE (test, initial_loc, type, i, 2, 5 + i, 5 + i);

  ASSERT_NUM_SUBSTRING_RANGES (test, initial_loc, type, 11);
}

/* Another test of string literal concatenation.  */

static void
test_lexer_string_locations_concatenation_2 (const line_table_case &case_)
{
  /* Digits 0-9.
     .....................000000000.111.11111112222222
     .....................123456789.012.34567890123456.  */
  const char *content = ("        \"01\" /* non-str */\n"
			 "        \"23\" /* non-str */\n"
			 "        \"45\" /* non-str */\n"
			 "        \"67\" /* non-str */\n"
			 "        \"89\" /* non-str */\n");
  lexer_test test (case_, content, NULL);

  auto_vec <cpp_string> input_strings;
  location_t input_locs[5];

  /* Verify that we get the expected tokens back.  */
  for (int i = 0; i < 5; i++)
    {
      const cpp_token *tok = test.get_token ();
      ASSERT_EQ (tok->type, CPP_STRING);
      input_strings.safe_push (tok->val.str);
      input_locs[i] = tok->src_loc;
    }

  /* Verify that cpp_interpret_string works.  */
  cpp_string dst_string;
  const enum cpp_ttype type = CPP_STRING;
  bool result = cpp_interpret_string (test.m_parser,
				      input_strings.address (), 5,
				      &dst_string, type);
  ASSERT_TRUE (result);
  ASSERT_STREQ ("0123456789", (const char *)dst_string.text);
  free (const_cast <unsigned char *> (dst_string.text));

  /* Simulate c-lex.cc's lex_string in order to record concatenation.  */
  test.m_concats.record_string_concatenation (5, input_locs);

  location_t initial_loc = input_locs[0];

  /* Within ASSERT_CHAR_AT_RANGE (actually assert_char_at_range), we can
     detect if the initial loc is after LINE_MAP_MAX_LOCATION_WITH_COLS
     and expect get_source_range_for_substring to fail.
     However, for a string concatenation test, we can have a case
     where the initial string is fully before LINE_MAP_MAX_LOCATION_WITH_COLS,
     but subsequent strings can be after it.
     Attempting to detect this within assert_char_at_range
     would overcomplicate the logic for the common test cases, so
     we detect it here.  */
  if (should_have_column_data_p (input_locs[0])
      && !should_have_column_data_p (input_locs[4]))
    {
      /* Verify that get_source_range_for_substring gracefully rejects
	 this case.  */
      source_range actual_range;
      const char *err
	= get_source_range_for_char (test.m_parser, test.m_file_cache,
				     &test.m_concats,
				     initial_loc, type, 0, &actual_range);
      ASSERT_STREQ ("range starts after LINE_MAP_MAX_LOCATION_WITH_COLS", err);
      return;
    }

  for (int i = 0; i < 5; i++)
    for (int j = 0; j < 2; j++)
      ASSERT_CHAR_AT_RANGE (test, initial_loc, type, (i * 2) + j,
			    i + 1, 10 + j, 10 + j);

  /* NUL-terminator should use the final closing quote at line 5 column 12.  */
  ASSERT_CHAR_AT_RANGE (test, initial_loc, type, 10, 5, 12, 12);

  ASSERT_NUM_SUBSTRING_RANGES (test, initial_loc, type, 11);
}

/* Another test of string literal concatenation, this time combined with
   various kinds of escaped characters.  */

static void
test_lexer_string_locations_concatenation_3 (const line_table_case &case_)
{
  /* Digits 0-9, expressing digit 5 in ASCII as hex "\x35"
     digit 6 in ASCII as octal "\066", concatenating multiple strings.  */
  const char *content
    /* .000000000.111111.111.1.2222.222.2.2233.333.3333.34444444444555
       .123456789.012345.678.9.0123.456.7.8901.234.5678.90123456789012. */
    = ("        \"01234\"  \"\\x35\"  \"\\066\"  \"789\" /* non-str */\n");
  lexer_test test (case_, content, NULL);

  auto_vec <cpp_string> input_strings;
  location_t input_locs[4];

  /* Verify that we get the expected tokens back.  */
  for (int i = 0; i < 4; i++)
    {
      const cpp_token *tok = test.get_token ();
      ASSERT_EQ (tok->type, CPP_STRING);
      input_strings.safe_push (tok->val.str);
      input_locs[i] = tok->src_loc;
    }

  /* Verify that cpp_interpret_string works.  */
  cpp_string dst_string;
  const enum cpp_ttype type = CPP_STRING;
  bool result = cpp_interpret_string (test.m_parser,
				      input_strings.address (), 4,
				      &dst_string, type);
  ASSERT_TRUE (result);
  ASSERT_STREQ ("0123456789", (const char *)dst_string.text);
  free (const_cast <unsigned char *> (dst_string.text));

  /* Simulate c-lex.cc's lex_string in order to record concatenation.  */
  test.m_concats.record_string_concatenation (4, input_locs);

  location_t initial_loc = input_locs[0];

  for (int i = 0; i <= 4; i++)
    ASSERT_CHAR_AT_RANGE (test, initial_loc, type, i, 1, 10 + i, 10 + i);
  ASSERT_CHAR_AT_RANGE (test, initial_loc, type, 5, 1, 19, 22);
  ASSERT_CHAR_AT_RANGE (test, initial_loc, type, 6, 1, 27, 30);
  for (int i = 7; i <= 9; i++)
    ASSERT_CHAR_AT_RANGE (test, initial_loc, type, i, 1, 28 + i, 28 + i);

  /* NUL-terminator should use the location of the final closing quote.  */
  ASSERT_CHAR_AT_RANGE (test, initial_loc, type, 10, 1, 38, 38);

  ASSERT_NUM_SUBSTRING_RANGES (test, initial_loc, type, 11);
}

/* Test of string literal in a macro.  */

static void
test_lexer_string_locations_macro (const line_table_case &case_)
{
  /* Digits 0-9.
     .....................0000000001111111111.22222222223.
     .....................1234567890123456789.01234567890.  */
  const char *content = ("#define MACRO     \"0123456789\" /* non-str */\n"
			 "  MACRO");
  lexer_test test (case_, content, NULL);

  /* Verify that we get the expected tokens back.  */
  const cpp_token *tok = test.get_token ();
  ASSERT_EQ (tok->type, CPP_PADDING);

  tok = test.get_token ();
  ASSERT_EQ (tok->type, CPP_STRING);
  ASSERT_TOKEN_AS_TEXT_EQ (test.m_parser, tok, "\"0123456789\"");

  /* Verify ranges of individual characters.  We ought to
     see columns within the macro definition.  */
  for (int i = 0; i <= 10; i++)
    ASSERT_CHAR_AT_RANGE (test, tok->src_loc, CPP_STRING,
			  i, 1, 20 + i, 20 + i);

  ASSERT_NUM_SUBSTRING_RANGES (test, tok->src_loc, CPP_STRING, 11);

  tok = test.get_token ();
  ASSERT_EQ (tok->type, CPP_PADDING);
}

/* Test of stringification of a macro argument.  */

static void
test_lexer_string_locations_stringified_macro_argument
  (const line_table_case &case_)
{
  /* .....................000000000111111111122222222223.
     .....................123456789012345678901234567890.  */
  const char *content = ("#define MACRO(X) #X /* non-str */\n"
			 "MACRO(foo)\n");
  lexer_test test (case_, content, NULL);

  /* Verify that we get the expected token back.  */
  const cpp_token *tok = test.get_token ();
  ASSERT_EQ (tok->type, CPP_PADDING);

  tok = test.get_token ();
  ASSERT_EQ (tok->type, CPP_STRING);
  ASSERT_TOKEN_AS_TEXT_EQ (test.m_parser, tok, "\"foo\"");

  /* We don't support getting the location of a stringified macro
     argument.  Verify that it fails gracefully.  */
  ASSERT_HAS_NO_SUBSTRING_RANGES (test, tok->src_loc, CPP_STRING,
				  "cpp_interpret_string_1 failed");

  tok = test.get_token ();
  ASSERT_EQ (tok->type, CPP_PADDING);

  tok = test.get_token ();
  ASSERT_EQ (tok->type, CPP_PADDING);
}

/* Ensure that we are fail gracefully if something attempts to pass
   in a location that isn't a string literal token.  Seen on this code:

     const char a[] = " %d ";
     __builtin_printf (a, 0.5);
                       ^

   when c-format.cc erroneously used the indicated one-character
   location as the format string location, leading to a read past the
   end of a string buffer in cpp_interpret_string_1.  */

static void
test_lexer_string_locations_non_string (const line_table_case &case_)
{
  /* .....................000000000111111111122222222223.
     .....................123456789012345678901234567890.  */
  const char *content = ("         a\n");
  lexer_test test (case_, content, NULL);

  /* Verify that we get the expected token back.  */
  const cpp_token *tok = test.get_token ();
  ASSERT_EQ (tok->type, CPP_NAME);
  ASSERT_TOKEN_AS_TEXT_EQ (test.m_parser, tok, "a");

  /* At this point, libcpp is attempting to interpret the name as a
     string literal, despite it not starting with a quote.  We don't detect
     that, but we should at least fail gracefully.  */
  ASSERT_HAS_NO_SUBSTRING_RANGES (test, tok->src_loc, CPP_STRING,
				  "cpp_interpret_string_1 failed");
}

/* Ensure that we can read substring information for a token which
   starts in one linemap and ends in another .  Adapted from
   gcc.dg/cpp/pr69985.c.  */

static void
test_lexer_string_locations_long_line (const line_table_case &case_)
{
  /* .....................000000.000111111111
     .....................123456.789012346789.  */
  const char *content = ("/* A very long line, so that we start a new line map.  */\n"
			 "     \"0123456789012345678901234567890123456789"
			 "0123456789012345678901234567890123456789"
			 "0123456789012345678901234567890123456789"
			 "0123456789\"\n");

  lexer_test test (case_, content, NULL);

  /* Verify that we get the expected token back.  */
  const cpp_token *tok = test.get_token ();
  ASSERT_EQ (tok->type, CPP_STRING);

  if (!should_have_column_data_p (line_table->highest_location))
    return;

  /* Verify ranges of individual characters.  */
  ASSERT_NUM_SUBSTRING_RANGES (test, tok->src_loc, CPP_STRING, 131);
  for (int i = 0; i < 131; i++)
    ASSERT_CHAR_AT_RANGE (test, tok->src_loc, CPP_STRING,
			  i, 2, 7 + i, 7 + i);
}

/* Test of locations within a raw string that doesn't contain a newline.  */

static void
test_lexer_string_locations_raw_string_one_line (const line_table_case &case_)
{
  /* .....................00.0000000111111111122.
     .....................12.3456789012345678901.  */
  const char *content = ("R\"foo(0123456789)foo\"\n");
  lexer_test test (case_, content, NULL);

  /* Verify that we get the expected token back.  */
  const cpp_token *tok = test.get_token ();
  ASSERT_EQ (tok->type, CPP_STRING);

  /* Verify that cpp_interpret_string works.  */
  cpp_string dst_string;
  const enum cpp_ttype type = CPP_STRING;
  bool result = cpp_interpret_string (test.m_parser, &tok->val.str, 1,
				      &dst_string, type);
  ASSERT_TRUE (result);
  ASSERT_STREQ ("0123456789", (const char *)dst_string.text);
  free (const_cast <unsigned char *> (dst_string.text));

  if (!should_have_column_data_p (line_table->highest_location))
    return;

  /* 0-9, plus the nil terminator.  */
  ASSERT_NUM_SUBSTRING_RANGES (test, tok->src_loc, CPP_STRING, 11);
  for (int i = 0; i < 11; i++)
    ASSERT_CHAR_AT_RANGE (test, tok->src_loc, CPP_STRING,
			  i, 1, 7 + i, 7 + i);
}

/* Test of locations within a raw string that contains a newline.  */

static void
test_lexer_string_locations_raw_string_multiline (const line_table_case &case_)
{
  /* .....................00.0000.
     .....................12.3456.  */
  const char *content = ("R\"foo(\n"
  /* .....................00000.
     .....................12345.  */
			 "hello\n"
			 "world\n"
  /* .....................00000.
     .....................12345.  */
			 ")foo\"\n");
  lexer_test test (case_, content, NULL);

  /* Verify that we get the expected token back.  */
  const cpp_token *tok = test.get_token ();
  ASSERT_EQ (tok->type, CPP_STRING);

  /* Verify that cpp_interpret_string works.  */
  cpp_string dst_string;
  const enum cpp_ttype type = CPP_STRING;
  bool result = cpp_interpret_string (test.m_parser, &tok->val.str, 1,
				      &dst_string, type);
  ASSERT_TRUE (result);
  ASSERT_STREQ ("\nhello\nworld\n", (const char *)dst_string.text);
  free (const_cast <unsigned char *> (dst_string.text));

  if (!should_have_column_data_p (line_table->highest_location))
    return;

  /* Currently we don't support locations within raw strings that
     contain newlines.  */
  ASSERT_HAS_NO_SUBSTRING_RANGES (test, tok->src_loc, tok->type,
				  "range endpoints are on different lines");
}

/* Test of parsing an unterminated raw string.  */

static void
test_lexer_string_locations_raw_string_unterminated (const line_table_case &case_)
{
  const char *content = "R\"ouch()ouCh\" /* etc */";

  lexer_diagnostic_sink diagnostics;
  lexer_test test (case_, content, &diagnostics);
  test.m_implicitly_expect_EOF = false;

  /* Attempt to parse the raw string.  */
  const cpp_token *tok = test.get_token ();
  ASSERT_EQ (tok->type, CPP_EOF);

  ASSERT_EQ (1, diagnostics.m_diagnostics.length ());
  /* We expect the message "unterminated raw string"
     in the "cpplib" translation domain.
     It's not clear that dgettext is available on all supported hosts,
     so this assertion is commented-out for now.
       ASSERT_STREQ (dgettext ("cpplib", "unterminated raw string"),
                     diagnostics.m_diagnostics[0]);
  */
}

/* Test of lexing char constants.  */

static void
test_lexer_char_constants (const line_table_case &case_)
{
  /* Various char constants.
     .....................0000000001111111111.22222222223.
     .....................1234567890123456789.01234567890.  */
  const char *content = ("         'a'\n"
			 "        u'a'\n"
			 "        U'a'\n"
			 "        L'a'\n"
			 "         'abc'\n");
  lexer_test test (case_, content, NULL);

  /* Verify that we get the expected tokens back.  */
  /* 'a'.  */
  const cpp_token *tok = test.get_token ();
  ASSERT_EQ (tok->type, CPP_CHAR);
  ASSERT_TOKEN_AS_TEXT_EQ (test.m_parser, tok, "'a'");

  unsigned int chars_seen;
  int unsignedp;
  cppchar_t cc = cpp_interpret_charconst (test.m_parser, tok,
					  &chars_seen, &unsignedp);
  ASSERT_EQ (cc, 'a');
  ASSERT_EQ (chars_seen, 1);

  /* u'a'.  */
  tok = test.get_token ();
  ASSERT_EQ (tok->type, CPP_CHAR16);
  ASSERT_TOKEN_AS_TEXT_EQ (test.m_parser, tok, "u'a'");

  /* U'a'.  */
  tok = test.get_token ();
  ASSERT_EQ (tok->type, CPP_CHAR32);
  ASSERT_TOKEN_AS_TEXT_EQ (test.m_parser, tok, "U'a'");

  /* L'a'.  */
  tok = test.get_token ();
  ASSERT_EQ (tok->type, CPP_WCHAR);
  ASSERT_TOKEN_AS_TEXT_EQ (test.m_parser, tok, "L'a'");

  /* 'abc' (c-char-sequence).  */
  tok = test.get_token ();
  ASSERT_EQ (tok->type, CPP_CHAR);
  ASSERT_TOKEN_AS_TEXT_EQ (test.m_parser, tok, "'abc'");
}
/* A table of interesting location_t values, giving one axis of our test
   matrix.  */

static const location_t boundary_locations[] = {
  /* Zero means "don't override the default values for a new line_table".  */
  0,

  /* An arbitrary non-zero value that isn't close to one of
     the boundary values below.  */
  0x10000,

  /* Values near LINE_MAP_MAX_LOCATION_WITH_PACKED_RANGES.  */
  LINE_MAP_MAX_LOCATION_WITH_PACKED_RANGES - 0x100,
  LINE_MAP_MAX_LOCATION_WITH_PACKED_RANGES - 1,
  LINE_MAP_MAX_LOCATION_WITH_PACKED_RANGES,
  LINE_MAP_MAX_LOCATION_WITH_PACKED_RANGES + 1,
  LINE_MAP_MAX_LOCATION_WITH_PACKED_RANGES + 0x100,

  /* Values near LINE_MAP_MAX_LOCATION_WITH_COLS.  */
  LINE_MAP_MAX_LOCATION_WITH_COLS - 0x200,
  LINE_MAP_MAX_LOCATION_WITH_COLS - 1,
  LINE_MAP_MAX_LOCATION_WITH_COLS,
  LINE_MAP_MAX_LOCATION_WITH_COLS + 1,
  LINE_MAP_MAX_LOCATION_WITH_COLS + 0x200,
};

/* Run TESTCASE multiple times, once for each case in our test matrix.  */

void
for_each_line_table_case (void (*testcase) (const line_table_case &))
{
  /* As noted above in the description of struct line_table_case,
     we want to explore a test matrix of interesting line_table
     situations, running various selftests for each case within the
     matrix.  */

  /* Run all tests with:
     (a) line_table->default_range_bits == 0, and
     (b) line_table->default_range_bits == line_map_suggested_range_bits.  */

  for (int default_range_bits: {0, line_map_suggested_range_bits})
    {
      /* ...and use each of the "interesting" location values as
	 the starting location within line_table.  */
      const int num_boundary_locations = ARRAY_SIZE (boundary_locations);
      for (int loc_idx = 0; loc_idx < num_boundary_locations; loc_idx++)
	{
	  line_table_case c (default_range_bits, boundary_locations[loc_idx]);
	  testcase (c);
	}
    }
}

/* Verify that when presented with a consecutive pair of locations with
   a very large line offset, we don't attempt to consolidate them into
   a single ordinary linemap where the line offsets within the line map
   would lead to overflow (PR lto/88147).  */

static void
test_line_offset_overflow ()
{
  line_table_test ltt (line_table_case (5, 0));

  linemap_add (line_table, LC_ENTER, false, "foo.c", 0);
  linemap_line_start (line_table, 1, 100);
  location_t loc_a = linemap_line_start (line_table, 2578, 255);
  assert_loceq ("foo.c", 2578, 0, loc_a);

  const line_map_ordinary *ordmap_a = LINEMAPS_LAST_ORDINARY_MAP (line_table);
  ASSERT_EQ (ordmap_a->m_column_and_range_bits, 13);
  ASSERT_EQ (ordmap_a->m_range_bits, 5);

  location_t loc_b = linemap_line_start (line_table, 404198, 512);
  assert_loceq ("foo.c", 404198, 0, loc_b);

  /* We should have started a new linemap, rather than attempting to store
     a very large line offset.  */
  const line_map_ordinary *ordmap_b = LINEMAPS_LAST_ORDINARY_MAP (line_table);
  ASSERT_NE (ordmap_a, ordmap_b);
}

void test_cpp_utf8 ()
{
  const int def_tabstop = 8;
  cpp_char_column_policy policy (def_tabstop, cpp_wcwidth);

  /* Verify that wcwidth of invalid UTF-8 or control bytes is 1.  */
  {
    int w_bad = cpp_display_width ("\xf0!\x9f!\x98!\x82!", 8, policy);
    ASSERT_EQ (8, w_bad);
    int w_ctrl = cpp_display_width ("\r\n\v\0\1", 5, policy);
    ASSERT_EQ (5, w_ctrl);
  }

  /* Verify that wcwidth of valid UTF-8 is as expected.  */
  {
    const int w_pi = cpp_display_width ("\xcf\x80", 2, policy);
    ASSERT_EQ (1, w_pi);
    const int w_emoji = cpp_display_width ("\xf0\x9f\x98\x82", 4, policy);
    ASSERT_EQ (2, w_emoji);
    const int w_umlaut_precomposed = cpp_display_width ("\xc3\xbf", 2,
							policy);
    ASSERT_EQ (1, w_umlaut_precomposed);
    const int w_umlaut_combining = cpp_display_width ("y\xcc\x88", 3,
						      policy);
    ASSERT_EQ (1, w_umlaut_combining);
    const int w_han = cpp_display_width ("\xe4\xb8\xba", 3, policy);
    ASSERT_EQ (2, w_han);
    const int w_ascii = cpp_display_width ("GCC", 3, policy);
    ASSERT_EQ (3, w_ascii);
    const int w_mixed = cpp_display_width ("\xcf\x80 = 3.14 \xf0\x9f\x98\x82"
					   "\x9f! \xe4\xb8\xba y\xcc\x88",
					   24, policy);
    ASSERT_EQ (18, w_mixed);
  }

  /* Verify that display width properly expands tabs.  */
  {
    const char *tstr = "\tabc\td";
    ASSERT_EQ (6, cpp_display_width (tstr, 6,
				     cpp_char_column_policy (1, cpp_wcwidth)));
    ASSERT_EQ (10, cpp_display_width (tstr, 6,
				      cpp_char_column_policy (3, cpp_wcwidth)));
    ASSERT_EQ (17, cpp_display_width (tstr, 6,
				      cpp_char_column_policy (8, cpp_wcwidth)));
    ASSERT_EQ (1,
	       cpp_display_column_to_byte_column
		 (tstr, 6, 7, cpp_char_column_policy (8, cpp_wcwidth)));
  }

  /* Verify that cpp_byte_column_to_display_column can go past the end,
     and similar edge cases.  */
  {
    const char *str
      /* Display columns.
         111111112345  */
      = "\xcf\x80 abc";
      /* 111122223456
	 Byte columns.  */

    ASSERT_EQ (5, cpp_display_width (str, 6, policy));
    ASSERT_EQ (105,
	       cpp_byte_column_to_display_column (str, 6, 106, policy));
    ASSERT_EQ (10000,
	       cpp_byte_column_to_display_column (NULL, 0, 10000, policy));
    ASSERT_EQ (0,
	       cpp_byte_column_to_display_column (NULL, 10000, 0, policy));
  }

  /* Verify that cpp_display_column_to_byte_column can go past the end,
     and similar edge cases, and check invertibility.  */
  {
    const char *str
      /* Display columns.
	 000000000000000000000000000000000000011
	 111111112222222234444444455555555678901  */
      = "\xf0\x9f\x98\x82 \xf0\x9f\x98\x82 hello";
      /* 000000000000000000000000000000000111111
	 111122223333444456666777788889999012345
	 Byte columns.  */
    ASSERT_EQ (4, cpp_display_column_to_byte_column (str, 15, 2, policy));
    ASSERT_EQ (15,
	       cpp_display_column_to_byte_column (str, 15, 11, policy));
    ASSERT_EQ (115,
	       cpp_display_column_to_byte_column (str, 15, 111, policy));
    ASSERT_EQ (10000,
	       cpp_display_column_to_byte_column (NULL, 0, 10000, policy));
    ASSERT_EQ (0,
	       cpp_display_column_to_byte_column (NULL, 10000, 0, policy));

    /* Verify that we do not interrupt a UTF-8 sequence.  */
    ASSERT_EQ (4, cpp_display_column_to_byte_column (str, 15, 1, policy));

    for (int byte_col = 1; byte_col <= 15; ++byte_col)
      {
	const int disp_col
	  = cpp_byte_column_to_display_column (str, 15, byte_col, policy);
	const int byte_col2
	  = cpp_display_column_to_byte_column (str, 15, disp_col, policy);

	/* If we ask for the display column in the middle of a UTF-8
	   sequence, it will return the length of the partial sequence,
	   matching the behavior of GCC before display column support.
	   Otherwise check the round trip was successful.  */
	if (byte_col < 4)
	  ASSERT_EQ (byte_col, disp_col);
	else if (byte_col >= 6 && byte_col < 9)
	  ASSERT_EQ (3 + (byte_col - 5), disp_col);
	else
	  ASSERT_EQ (byte_col2, byte_col);
      }
  }
}

static bool
check_cpp_valid_utf8_p (const char *str)
{
  return cpp_valid_utf8_p (str, strlen (str));
}

/* Check that cpp_valid_utf8_p works as expected.  */

static void
test_cpp_valid_utf8_p ()
{
  ASSERT_TRUE (check_cpp_valid_utf8_p ("hello world"));

  /* 2-byte char (pi).  */
  ASSERT_TRUE (check_cpp_valid_utf8_p("\xcf\x80"));

  /* 3-byte chars (the Japanese word "mojibake").  */
  ASSERT_TRUE (check_cpp_valid_utf8_p
	       (
		/* U+6587 CJK UNIFIED IDEOGRAPH-6587
		   UTF-8: 0xE6 0x96 0x87
		   C octal escaped UTF-8: \346\226\207.  */
		"\346\226\207"
		/* U+5B57 CJK UNIFIED IDEOGRAPH-5B57
		   UTF-8: 0xE5 0xAD 0x97
		   C octal escaped UTF-8: \345\255\227.  */
		"\345\255\227"
		/* U+5316 CJK UNIFIED IDEOGRAPH-5316
		   UTF-8: 0xE5 0x8C 0x96
		   C octal escaped UTF-8: \345\214\226.  */
		"\345\214\226"
		/* U+3051 HIRAGANA LETTER KE
		   UTF-8: 0xE3 0x81 0x91
		   C octal escaped UTF-8: \343\201\221.  */
		"\343\201\221"));

  /* 4-byte char: an emoji.  */
  ASSERT_TRUE (check_cpp_valid_utf8_p ("\xf0\x9f\x98\x82"));

  /* Control codes, including the NUL byte.  */
  ASSERT_TRUE (cpp_valid_utf8_p ("\r\n\v\0\1", 5));

  ASSERT_FALSE (check_cpp_valid_utf8_p ("\xf0!\x9f!\x98!\x82!"));

  /* Unexpected continuation bytes.  */
  for (unsigned char continuation_byte = 0x80;
       continuation_byte <= 0xbf;
       continuation_byte++)
    ASSERT_FALSE (cpp_valid_utf8_p ((const char *)&continuation_byte, 1));

  /* "Lonely start characters" for 2-byte sequences.  */
  {
    unsigned char buf[2];
    buf[1] = ' ';
    for (buf[0] = 0xc0;
	 buf[0] <= 0xdf;
	 buf[0]++)
      ASSERT_FALSE (cpp_valid_utf8_p ((const char *)buf, 2));
  }

  /* "Lonely start characters" for 3-byte sequences.  */
  {
    unsigned char buf[2];
    buf[1] = ' ';
    for (buf[0] = 0xe0;
	 buf[0] <= 0xef;
	 buf[0]++)
      ASSERT_FALSE (cpp_valid_utf8_p ((const char *)buf, 2));
  }

  /* "Lonely start characters" for 4-byte sequences.  */
  {
    unsigned char buf[2];
    buf[1] = ' ';
    for (buf[0] = 0xf0;
	 buf[0] <= 0xf4;
	 buf[0]++)
      ASSERT_FALSE (cpp_valid_utf8_p ((const char *)buf, 2));
  }

  /* Invalid start characters (formerly valid for 5-byte and 6-byte
     sequences).  */
  {
    unsigned char buf[2];
    buf[1] = ' ';
    for (buf[0] = 0xf5;
	 buf[0] <= 0xfd;
	 buf[0]++)
      ASSERT_FALSE (cpp_valid_utf8_p ((const char *)buf, 2));
  }

  /* Impossible bytes.  */
  ASSERT_FALSE (check_cpp_valid_utf8_p ("\xc0"));
  ASSERT_FALSE (check_cpp_valid_utf8_p ("\xc1"));
  ASSERT_FALSE (check_cpp_valid_utf8_p ("\xfe"));
  ASSERT_FALSE (check_cpp_valid_utf8_p ("\xff"));
}

/* Run all of the selftests within this file.  */

void
input_cc_tests ()
{
  test_linenum_comparisons ();
  test_should_have_column_data_p ();
  test_unknown_location ();
  test_builtins ();
  for_each_line_table_case (test_make_location_nonpure_range_endpoints);

  for_each_line_table_case (test_accessing_ordinary_linemaps);
  for_each_line_table_case (test_lexer);
  for_each_line_table_case (test_lexer_string_locations_simple);
  for_each_line_table_case (test_lexer_string_locations_ebcdic);
  for_each_line_table_case (test_lexer_string_locations_hex);
  for_each_line_table_case (test_lexer_string_locations_oct);
  for_each_line_table_case (test_lexer_string_locations_letter_escape_1);
  for_each_line_table_case (test_lexer_string_locations_letter_escape_2);
  for_each_line_table_case (test_lexer_string_locations_ucn4);
  for_each_line_table_case (test_lexer_string_locations_ucn8);
  for_each_line_table_case (test_lexer_string_locations_wide_string);
  for_each_line_table_case (test_lexer_string_locations_string16);
  for_each_line_table_case (test_lexer_string_locations_string32);
  for_each_line_table_case (test_lexer_string_locations_u8);
  for_each_line_table_case (test_lexer_string_locations_utf8_source);
  for_each_line_table_case (test_lexer_string_locations_concatenation_1);
  for_each_line_table_case (test_lexer_string_locations_concatenation_2);
  for_each_line_table_case (test_lexer_string_locations_concatenation_3);
  for_each_line_table_case (test_lexer_string_locations_macro);
  for_each_line_table_case (test_lexer_string_locations_stringified_macro_argument);
  for_each_line_table_case (test_lexer_string_locations_non_string);
  for_each_line_table_case (test_lexer_string_locations_long_line);
  for_each_line_table_case (test_lexer_string_locations_raw_string_one_line);
  for_each_line_table_case (test_lexer_string_locations_raw_string_multiline);
  for_each_line_table_case (test_lexer_string_locations_raw_string_unterminated);
  for_each_line_table_case (test_lexer_char_constants);

  test_reading_source_line ();
  test_reading_source_buffer ();

  test_line_offset_overflow ();

  test_cpp_utf8 ();
  test_cpp_valid_utf8_p ();
}

} // namespace selftest

#endif /* CHECKING_P */
