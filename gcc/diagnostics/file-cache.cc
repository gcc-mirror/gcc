/* Caching input files for use by diagnostics.
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
#include "cpplib.h"
#include "diagnostics/file-cache.h"
#include "diagnostics/dumping.h"
#include "selftest.h"

#ifndef HAVE_ICONV
#define HAVE_ICONV 0
#endif

namespace diagnostics {

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

  static size_t tune (size_t line_record_size_)
  {
    size_t ret = line_record_size;
    line_record_size = line_record_size_;
    return ret;
  }

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

    static bool less_than(const line_info &a, const line_info &b)
    {
      return a.line_num < b.line_num;
    }
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
  static size_t line_record_size;
  static size_t recent_cached_lines_shift;

  /* The number of time this file has been accessed.  This is used
     to designate which file cache to evict from the cache
     array.  */
  unsigned m_use_count;

  /* The file_path is the key for identifying a particular file in
     the cache.  This copy is owned by the slot.  */
  char *m_file_path;

  FILE *m_fp;

  /* True when an read error happened.  */
  bool m_error;

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

  /* Could this file be missing a trailing newline on its final line?
     Initially true (to cope with empty files), set to true/false
     as each line is read.  */
  bool m_missing_trailing_newline;

  /* This is a record of the beginning and end of the lines we've seen
     while reading the file.  This is useful to avoid walking the data
     from the beginning when we are asked to read a line that is
     before LINE_START_IDX above.  When the lines exceed line_record_size
     this is scaled down dynamically, with the line_info becoming anchors.  */
  vec<line_info, va_heap> m_line_record;

  /* A cache of the recently seen lines. This is maintained as a ring
     buffer. */
  vec<line_info, va_heap> m_line_recent;

  /* First and last valid entry in m_line_recent.  */
  size_t m_line_recent_last, m_line_recent_first;

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

size_t file_cache_slot::line_record_size = 0;
size_t file_cache_slot::recent_cached_lines_shift = 8;

/* Tune file_cache.  */
void
file_cache::tune (size_t num_file_slots, size_t lines)
{
  if (file_cache_slot::tune (lines) != lines
      || m_num_file_slots != num_file_slots)
    {
      delete[] m_file_slots;
      m_file_slots = new file_cache_slot[num_file_slots];
    }
  m_num_file_slots = num_file_slots;
}

static const char *
find_end_of_line (const char *s, size_t len);

/* Lookup the cache used for the content of a given file accessed by
   caret diagnostic.  Return the found cached file, or NULL if no
   cached file was found.  */

file_cache_slot *
file_cache::lookup_file (const char *file_path)
{
  gcc_assert (file_path);

  /* This will contain the found cached file.  */
  file_cache_slot *r = NULL;
  for (unsigned i = 0; i < m_num_file_slots; ++i)
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
  free (m_file_path);
  m_file_path = NULL;
  if (m_fp)
    fclose (m_fp);
  m_error = false;
  m_fp = NULL;
  m_nb_read = 0;
  m_line_start_idx = 0;
  m_line_num = 0;
  m_line_record.truncate (0);
  m_line_recent_first = 0;
  m_line_recent_last = 0;
  m_use_count = 0;
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
  for (unsigned i = 1; i < m_num_file_slots; ++i)
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
   m_num_file_slots files are cached.

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
  m_file_path = file_path ? xstrdup (file_path) : nullptr;
  if (m_fp)
    fclose (m_fp);
  m_error = false;
  m_fp = fp;
  if (m_alloc_offset)
    offset_buffer (-m_alloc_offset);
  m_nb_read = 0;
  m_line_start_idx = 0;
  m_line_num = 0;
  m_line_recent_first = 0;
  m_line_recent_last = 0;
  m_line_record.truncate (0);
  /* Ensure that this cache entry doesn't get evicted next time
     add_file_to_cache_tab is called.  */
  m_use_count = ++highest_use_count;
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
}

/* file_cache's ctor.  */

file_cache::file_cache ()
: m_num_file_slots (16), m_file_slots (new file_cache_slot[m_num_file_slots])
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
  for (size_t i = 0; i < m_num_file_slots; ++i)
    {
      dumping::emit_indent (out, indent);
      fprintf (out, "slot[%i]:\n", (int)i);
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
: m_use_count (0), m_file_path (NULL), m_fp (NULL), m_error (false), m_data (0),
  m_alloc_offset (0), m_size (0), m_nb_read (0), m_line_start_idx (0),
  m_line_num (0), m_missing_trailing_newline (true),
  m_line_recent_last (0), m_line_recent_first (0)
{
  m_line_record.create (0);
  m_line_recent.create (1U << recent_cached_lines_shift);
  for (int i = 0; i < 1 << recent_cached_lines_shift; i++)
    m_line_recent.quick_push (file_cache_slot::line_info (0, 0, 0));
}

/* Destructor for a cache of file used by caret diagnostic.  */

file_cache_slot::~file_cache_slot ()
{
  free (m_file_path);
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
  m_line_recent.release ();
}

void
file_cache_slot::dump (FILE *out, int indent) const
{
  if (!m_file_path)
    {
      dumping::emit_indent (out, indent);
      fprintf (out, "(unused)\n");
      return;
    }
  dumping::emit_string_field (out, indent, "file_path", m_file_path);
  {
    dumping::emit_indent (out, indent);
    fprintf (out, "fp: %p\n", (void *)m_fp);
  }
  dumping::emit_bool_field (out, indent, "needs_read_p", needs_read_p ());
  dumping::emit_bool_field (out, indent, "needs_grow_p", needs_grow_p ());
  dumping::emit_unsigned_field (out, indent, "use_count", m_use_count);
  dumping::emit_size_t_field (out, indent, "size", m_size);
  dumping::emit_size_t_field (out, indent, "nb_read", m_nb_read);
  dumping::emit_size_t_field (out, indent, "start_line_idx", m_line_start_idx);
  dumping::emit_size_t_field (out, indent, "line_num", m_line_num);
  dumping::emit_bool_field (out, indent, "missing_trailing_newline",
			    m_missing_trailing_newline);
  {
    dumping::emit_indent (out, indent);
    fprintf (out, "line records (%i):\n", m_line_record.length ());
  }
  int idx = 0;
  for (auto &line : m_line_record)
    {
      dumping::emit_indent (out, indent);
      fprintf (out, ("[%i]:"
		     " line " HOST_SIZE_T_PRINT_DEC ":"
		     " byte offsets: " HOST_SIZE_T_PRINT_DEC
		     "-" HOST_SIZE_T_PRINT_DEC "\n"),
	       idx++, line.line_num, line.start_pos, line.end_pos);
    }
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
    {
      m_error = true;
      return false;
    }

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

  if (m_error)
    return false;

  /* At this point, we've found the end of the of line.  It either points to
     the line terminator or to one byte after the last byte of the file.  */
  gcc_assert (line_end != NULL);

  len = line_end - line_start;

  if (m_line_start_idx < m_nb_read)
    *line = const_cast<char *> (line_start);

  ++m_line_num;

  /* Now update our line record so that re-reading lines from the
     before m_line_start_idx is faster.  */
  size_t rlen = m_line_record.length ();
  /* Only update when beyond the previously cached region.  */
  if (rlen == 0 || m_line_record[rlen - 1].line_num < m_line_num)
    {
      size_t spacing
	= (rlen >= 2
	   ? (m_line_record[rlen - 1].line_num
	      - m_line_record[rlen - 2].line_num) : 1);
      size_t delta
	= rlen >= 1 ? m_line_num - m_line_record[rlen - 1].line_num : 1;

      size_t max_size = line_record_size;
      /* One anchor per hundred input lines.  */
      if (max_size == 0)
	max_size = m_line_num / 100;

      /* If we're too far beyond drop half of the lines to rebalance.  */
      if (rlen == max_size && delta >= spacing * 2)
	{
	  size_t j = 0;
	  for (size_t i = 1; i < rlen; i += 2)
	    m_line_record[j++] = m_line_record[i];
	  m_line_record.truncate (j);
	  rlen = j;
	  spacing *= 2;
	}

      if (rlen < max_size && delta >= spacing)
	{
	  file_cache_slot::line_info li (m_line_num, m_line_start_idx,
					 line_end - m_data);
	  m_line_record.safe_push (li);
	}
    }

  /* Cache recent tail lines separately for fast access. This assumes
     most accesses do not skip backwards.  */
  if (m_line_recent_last == m_line_recent_first
      || m_line_recent[m_line_recent_last].line_num == m_line_num - 1)
    {
      size_t mask = ((size_t) 1 << recent_cached_lines_shift) - 1;
      m_line_recent_last = (m_line_recent_last + 1) & mask;
      if (m_line_recent_last == m_line_recent_first)
	m_line_recent_first = (m_line_recent_first + 1) & mask;
      m_line_recent[m_line_recent_last]
	= file_cache_slot::line_info (m_line_num, m_line_start_idx,
				      line_end - m_data);
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

  /* Is the line in the recent line cache?
     This assumes the main file processing is only using
     a single contiguous cursor with only temporary excursions.  */
  if (m_line_recent_first != m_line_recent_last
	&& m_line_recent[m_line_recent_first].line_num <= line_num
	&& m_line_recent[m_line_recent_last].line_num >= line_num)
    {
      line_info &last = m_line_recent[m_line_recent_last];
      size_t mask = (1U << recent_cached_lines_shift) - 1;
      size_t idx = (m_line_recent_last - (last.line_num - line_num)) & mask;
      line_info &recent = m_line_recent[idx];
      gcc_assert (recent.line_num == line_num);
      *line = m_data + recent.start_pos;
      *line_len = recent.end_pos - recent.start_pos;
      return true;
    }

  if (line_num <= m_line_num)
    {
      line_info l (line_num, 0, 0);
      int i = m_line_record.lower_bound (l, line_info::less_than);
      if (i == 0)
	{
	  m_line_start_idx = 0;
	  m_line_num = 0;
	}
      else if (m_line_record[i - 1].line_num == line_num)
	{
	  /* We have the start/end of the line.  */
	  *line = m_data + m_line_record[i - 1].start_pos;
	  *line_len = m_line_record[i - 1].end_pos - m_line_record[i - 1].start_pos;
	  return true;
	}
      else
       {
	 gcc_assert (m_line_record[i - 1].line_num < m_line_num);
	 m_line_start_idx = m_line_record[i - 1].start_pos;
	 m_line_num = m_line_record[i - 1].line_num - 1;
       }
    }

  /*  Let's walk from line m_line_num up to line_num - 1, without
      copying any line.  */
  while (m_line_num < line_num - 1)
    if (!goto_next_line ())
      return false;

  /* The line we want is the next one.  Let's read it.  */
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

char_span
file_cache::get_source_file_content (const char *file_path)
{
  file_cache_slot *c = lookup_or_add_file (file_path);
  if (c == nullptr)
    return char_span (nullptr, 0);
  return c->get_full_file_content ();
}

#if CHECKING_P

namespace selftest {

 using temp_source_file = ::selftest::temp_source_file;

/* Verify reading of a specific line LINENUM in TMP, FC.  */

static void
check_line (temp_source_file &tmp, file_cache &fc, int linenum)
{
  char_span line = fc.get_source_line (tmp.get_filename (), linenum);
  int n;
  const char *b = line.get_buffer ();
  size_t l = line.length ();
  char buf[5];
  ASSERT_LT (l, 5);
  memcpy (buf, b, l);
  buf[l] = '\0';
  ASSERT_TRUE (sscanf (buf, "%d", &n) == 1);
  ASSERT_EQ (n, linenum);
}

/* Test file cache replacement.  */

static void
test_replacement ()
{
  const int maxline = 1000;

  char *vec = XNEWVEC (char, maxline * 5);
  char *p = vec;
  int i;
  for (i = 1; i <= maxline; i++)
    p += sprintf (p, "%d\n", i);

  temp_source_file tmp (SELFTEST_LOCATION, ".txt", vec);
  free (vec);
  file_cache fc;

  for (i = 2; i <= maxline; i++)
    {
      check_line (tmp, fc, i);
      check_line (tmp, fc, i - 1);
      if (i >= 10)
	check_line (tmp, fc, i - 9);
      if (i >= 350) /* Exceed the look behind cache.  */
	check_line (tmp, fc, i - 300);
    }
  for (i = 5; i <= maxline; i += 100)
    check_line (tmp, fc, i);
  for (i = 1; i <= maxline; i++)
    check_line (tmp, fc, i);
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

/* Run all of the selftests within this file.  */

void
file_cache_cc_tests ()
{
  test_reading_source_line ();
  test_reading_source_buffer ();
  test_replacement ();
}

} // namespace selftest

#endif /* CHECKING_P */

} // namespace diagnostics
