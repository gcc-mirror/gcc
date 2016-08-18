/* Data and functions related to line maps and input files.
   Copyright (C) 2004-2016 Free Software Foundation, Inc.

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
#include "diagnostic-core.h"
#include "selftest.h"
#include "cpplib.h"

#ifndef HAVE_ICONV
#define HAVE_ICONV 0
#endif

/* This is a cache used by get_next_line to store the content of a
   file to be searched for file lines.  */
struct fcache
{
  /* These are information used to store a line boundary.  */
  struct line_info
  {
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

  /* The number of time this file has been accessed.  This is used
     to designate which file cache to evict from the cache
     array.  */
  unsigned use_count;

  const char *file_path;

  FILE *fp;

  /* This points to the content of the file that we've read so
     far.  */
  char *data;

  /*  The size of the DATA array above.*/
  size_t size;

  /* The number of bytes read from the underlying file so far.  This
     must be less (or equal) than SIZE above.  */
  size_t nb_read;

  /* The index of the beginning of the current line.  */
  size_t line_start_idx;

  /* The number of the previous line read.  This starts at 1.  Zero
     means we've read no line so far.  */
  size_t line_num;

  /* This is the total number of lines of the current file.  At the
     moment, we try to get this information from the line map
     subsystem.  Note that this is just a hint.  When using the C++
     front-end, this hint is correct because the input file is then
     completely tokenized before parsing starts; so the line map knows
     the number of lines before compilation really starts.  For e.g,
     the C front-end, it can happen that we start emitting diagnostics
     before the line map has seen the end of the file.  */
  size_t total_lines;

  /* This is a record of the beginning and end of the lines we've seen
     while reading the file.  This is useful to avoid walking the data
     from the beginning when we are asked to read a line that is
     before LINE_START_IDX above.  Note that the maximum size of this
     record is fcache_line_record_size, so that the memory consumption
     doesn't explode.  We thus scale total_lines down to
     fcache_line_record_size.  */
  vec<line_info, va_heap> line_record;

  fcache ();
  ~fcache ();
};

/* Current position in real source file.  */

location_t input_location = UNKNOWN_LOCATION;

struct line_maps *line_table;

/* A stashed copy of "line_table" for use by selftest::line_table_test.
   This needs to be a global so that it can be a GC root, and thus
   prevent the stashed copy from being garbage-collected if the GC runs
   during a line_table_test.  */

struct line_maps *saved_line_table;

static fcache *fcache_tab;
static const size_t fcache_tab_size = 16;
static const size_t fcache_buffer_size = 4 * 1024;
static const size_t fcache_line_record_size = 100;

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
   code.  */

static expanded_location
expand_location_1 (source_location loc,
		   bool expansion_point_p)
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
	  loc = linemap_unwind_to_first_non_reserved_loc (line_table,
							  loc, NULL);
	  lrk = LRK_SPELLING_LOCATION;
	}
      loc = linemap_resolve_location (line_table, loc,
				      lrk, &map);
      xloc = linemap_expand_location (line_table, map, loc);
    }

  xloc.data = block;
  if (loc <= BUILTINS_LOCATION)
    xloc.file = loc == UNKNOWN_LOCATION ? NULL : _("<built-in>");

  return xloc;
}

/* Initialize the set of cache used for files accessed by caret
   diagnostic.  */

static void
diagnostic_file_cache_init (void)
{
  if (fcache_tab == NULL)
    fcache_tab = new fcache[fcache_tab_size];
}

/* Free the resources used by the set of cache used for files accessed
   by caret diagnostic.  */

void
diagnostic_file_cache_fini (void)
{
  if (fcache_tab)
    {
      delete [] (fcache_tab);
      fcache_tab = NULL;
    }
}

/* Return the total lines number that have been read so far by the
   line map (in the preprocessor) so far.  For languages like C++ that
   entirely preprocess the input file before starting to parse, this
   equals the actual number of lines of the file.  */

static size_t
total_lines_num (const char *file_path)
{
  size_t r = 0;
  source_location l = 0;
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

static fcache*
lookup_file_in_cache_tab (const char *file_path)
{
  if (file_path == NULL)
    return NULL;

  diagnostic_file_cache_init ();

  /* This will contain the found cached file.  */
  fcache *r = NULL;
  for (unsigned i = 0; i < fcache_tab_size; ++i)
    {
      fcache *c = &fcache_tab[i];
      if (c->file_path && !strcmp (c->file_path, file_path))
	{
	  ++c->use_count;
	  r = c;
	}
    }

  if (r)
    ++r->use_count;

  return r;
}

/* Purge any mention of FILENAME from the cache of files used for
   printing source code.  For use in selftests when working
   with tempfiles.  */

void
diagnostics_file_cache_forcibly_evict_file (const char *file_path)
{
  gcc_assert (file_path);

  fcache *r = lookup_file_in_cache_tab (file_path);
  if (!r)
    /* Not found.  */
    return;

  r->file_path = NULL;
  if (r->fp)
    fclose (r->fp);
  r->fp = NULL;
  r->nb_read = 0;
  r->line_start_idx = 0;
  r->line_num = 0;
  r->line_record.truncate (0);
  r->use_count = 0;
  r->total_lines = 0;
}

/* Return the file cache that has been less used, recently, or the
   first empty one.  If HIGHEST_USE_COUNT is non-null,
   *HIGHEST_USE_COUNT is set to the highest use count of the entries
   in the cache table.  */

static fcache*
evicted_cache_tab_entry (unsigned *highest_use_count)
{
  diagnostic_file_cache_init ();

  fcache *to_evict = &fcache_tab[0];
  unsigned huc = to_evict->use_count;
  for (unsigned i = 1; i < fcache_tab_size; ++i)
    {
      fcache *c = &fcache_tab[i];
      bool c_is_empty = (c->file_path == NULL);

      if (c->use_count < to_evict->use_count
	  || (to_evict->file_path && c_is_empty))
	/* We evict C because it's either an entry with a lower use
	   count or one that is empty.  */
	to_evict = c;

      if (huc < c->use_count)
	huc = c->use_count;

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
   fcache_tab_size files are cached.  */

static fcache*
add_file_to_cache_tab (const char *file_path)
{

  FILE *fp = fopen (file_path, "r");
  if (fp == NULL)
    return NULL;

  unsigned highest_use_count = 0;
  fcache *r = evicted_cache_tab_entry (&highest_use_count);
  r->file_path = file_path;
  if (r->fp)
    fclose (r->fp);
  r->fp = fp;
  r->nb_read = 0;
  r->line_start_idx = 0;
  r->line_num = 0;
  r->line_record.truncate (0);
  /* Ensure that this cache entry doesn't get evicted next time
     add_file_to_cache_tab is called.  */
  r->use_count = ++highest_use_count;
  r->total_lines = total_lines_num (file_path);

  return r;
}

/* Lookup the cache used for the content of a given file accessed by
   caret diagnostic.  If no cached file was found, create a new cache
   for this file, add it to the array of cached file and return
   it.  */

static fcache*
lookup_or_add_file_to_cache_tab (const char *file_path)
{
  fcache *r = lookup_file_in_cache_tab (file_path);
  if (r == NULL)
    r = add_file_to_cache_tab (file_path);
  return r;
}

/* Default constructor for a cache of file used by caret
   diagnostic.  */

fcache::fcache ()
: use_count (0), file_path (NULL), fp (NULL), data (0),
  size (0), nb_read (0), line_start_idx (0), line_num (0),
  total_lines (0)
{
  line_record.create (0);
}

/* Destructor for a cache of file used by caret diagnostic.  */

fcache::~fcache ()
{
  if (fp)
    {
      fclose (fp);
      fp = NULL;
    }
  if (data)
    {
      XDELETEVEC (data);
      data = 0;
    }
  line_record.release ();
}

/* Returns TRUE iff the cache would need to be filled with data coming
   from the file.  That is, either the cache is empty or full or the
   current line is empty.  Note that if the cache is full, it would
   need to be extended and filled again.  */

static bool
needs_read (fcache *c)
{
  return (c->nb_read == 0
	  || c->nb_read == c->size
	  || (c->line_start_idx >= c->nb_read - 1));
}

/*  Return TRUE iff the cache is full and thus needs to be
    extended.  */

static bool
needs_grow (fcache *c)
{
  return c->nb_read == c->size;
}

/* Grow the cache if it needs to be extended.  */

static void
maybe_grow (fcache *c)
{
  if (!needs_grow (c))
    return;

  size_t size = c->size == 0 ? fcache_buffer_size : c->size * 2;
  c->data = XRESIZEVEC (char, c->data, size + 1);
  c->size = size;
}

/*  Read more data into the cache.  Extends the cache if need be.
    Returns TRUE iff new data could be read.  */

static bool
read_data (fcache *c)
{
  if (feof (c->fp) || ferror (c->fp))
    return false;

  maybe_grow (c);

  char * from = c->data + c->nb_read;
  size_t to_read = c->size - c->nb_read;
  size_t nb_read = fread (from, 1, to_read, c->fp);

  if (ferror (c->fp))
    return false;

  c->nb_read += nb_read;
  return !!nb_read;
}

/* Read new data iff the cache needs to be filled with more data
   coming from the file FP.  Return TRUE iff the cache was filled with
   mode data.  */

static bool
maybe_read_data (fcache *c)
{
  if (!needs_read (c))
    return false;
  return read_data (c);
}

/* Read a new line from file FP, using C as a cache for the data
   coming from the file.  Upon successful completion, *LINE is set to
   the beginning of the line found.  Space for that line has been
   allocated in the cache thus *LINE has the same life time as C.
   *LINE_LEN is set to the length of the line.  Note that the line
   does not contain any terminal delimiter.  This function returns
   true if some data was read or process from the cache, false
   otherwise.  Note that subsequent calls to get_next_line return the
   next lines of the file and might overwrite the content of
   *LINE.  */

static bool
get_next_line (fcache *c, char **line, ssize_t *line_len)
{
  /* Fill the cache with data to process.  */
  maybe_read_data (c);

  size_t remaining_size = c->nb_read - c->line_start_idx;
  if (remaining_size == 0)
    /* There is no more data to process.  */
    return false;

  char *line_start = c->data + c->line_start_idx;

  char *next_line_start = NULL;
  size_t len = 0;
  char *line_end = (char *) memchr (line_start, '\n', remaining_size);
  if (line_end == NULL)
    {
      /* We haven't found the end-of-line delimiter in the cache.
	 Fill the cache with more data from the file and look for the
	 '\n'.  */
      while (maybe_read_data (c))
	{
	  line_start = c->data + c->line_start_idx;
	  remaining_size = c->nb_read - c->line_start_idx;
	  line_end = (char *) memchr (line_start, '\n', remaining_size);
	  if (line_end != NULL)
	    {
	      next_line_start = line_end + 1;
	      break;
	    }
	}
      if (line_end == NULL)
	/* We've loadded all the file into the cache and still no
	   '\n'.  Let's say the line ends up at one byte passed the
	   end of the file.  This is to stay consistent with the case
	   of when the line ends up with a '\n' and line_end points to
	   that terminal '\n'.  That consistency is useful below in
	   the len calculation.  */
	line_end = c->data + c->nb_read ;
    }
  else
    next_line_start = line_end + 1;

  if (ferror (c->fp))
    return -1;

  /* At this point, we've found the end of the of line.  It either
     points to the '\n' or to one byte after the last byte of the
     file.  */
  gcc_assert (line_end != NULL);

  len = line_end - line_start;

  if (c->line_start_idx < c->nb_read)
    *line = line_start;

  ++c->line_num;

  /* Before we update our line record, make sure the hint about the
     total number of lines of the file is correct.  If it's not, then
     we give up recording line boundaries from now on.  */
  bool update_line_record = true;
  if (c->line_num > c->total_lines)
    update_line_record = false;

    /* Now update our line record so that re-reading lines from the
     before c->line_start_idx is faster.  */
  if (update_line_record
      && c->line_record.length () < fcache_line_record_size)
    {
      /* If the file lines fits in the line record, we just record all
	 its lines ...*/
      if (c->total_lines <= fcache_line_record_size
	  && c->line_num > c->line_record.length ())
	c->line_record.safe_push (fcache::line_info (c->line_num,
						 c->line_start_idx,
						 line_end - c->data));
      else if (c->total_lines > fcache_line_record_size)
	{
	  /* ... otherwise, we just scale total_lines down to
	     (fcache_line_record_size lines.  */
	  size_t n = (c->line_num * fcache_line_record_size) / c->total_lines;
	  if (c->line_record.length () == 0
	      || n >= c->line_record.length ())
	    c->line_record.safe_push (fcache::line_info (c->line_num,
						     c->line_start_idx,
						     line_end - c->data));
	}
    }

  /* Update c->line_start_idx so that it points to the next line to be
     read.  */
  if (next_line_start)
    c->line_start_idx = next_line_start - c->data;
  else
    /* We didn't find any terminal '\n'.  Let's consider that the end
       of line is the end of the data in the cache.  The next
       invocation of get_next_line will either read more data from the
       underlying file or return false early because we've reached the
       end of the file.  */
    c->line_start_idx = c->nb_read;

  *line_len = len;

  return true;
}

/* Reads the next line from FILE into *LINE.  If *LINE is too small
   (or NULL) it is allocated (or extended) to have enough space to
   containe the line.  *LINE_LENGTH must contain the size of the
   initial*LINE buffer.  It's then updated by this function to the
   actual length of the returned line.  Note that the returned line
   can contain several zero bytes.  Also note that the returned string
   is allocated in static storage that is going to be re-used by
   subsequent invocations of read_line.  */

static bool
read_next_line (fcache *cache, char ** line, ssize_t *line_len)
{
  char *l = NULL;
  ssize_t len = 0;

  if (!get_next_line (cache, &l, &len))
    return false;

  if (*line == NULL)
    *line = XNEWVEC (char, len);
  else
    if (*line_len < len)
	*line = XRESIZEVEC (char, *line, len);

  memcpy (*line, l, len);
  *line_len = len;

  return true;
}

/* Consume the next bytes coming from the cache (or from its
   underlying file if there are remaining unread bytes in the file)
   until we reach the next end-of-line (or end-of-file).  There is no
   copying from the cache involved.  Return TRUE upon successful
   completion.  */

static bool
goto_next_line (fcache *cache)
{
  char *l;
  ssize_t len;

  return get_next_line (cache, &l, &len);
}

/* Read an arbitrary line number LINE_NUM from the file cached in C.
   The line is copied into *LINE.  *LINE_LEN must have been set to the
   length of *LINE.  If *LINE is too small (or NULL) it's extended (or
   allocated) and *LINE_LEN is adjusted accordingly.  *LINE ends up
   with a terminal zero byte and can contain additional zero bytes.
   This function returns bool if a line was read.  */

static bool
read_line_num (fcache *c, size_t line_num,
	       char ** line, ssize_t *line_len)
{
  gcc_assert (line_num > 0);

  if (line_num <= c->line_num)
    {
      /* We've been asked to read lines that are before c->line_num.
	 So lets use our line record (if it's not empty) to try to
	 avoid re-reading the file from the beginning again.  */

      if (c->line_record.is_empty ())
	{
	  c->line_start_idx = 0;
	  c->line_num = 0;
	}
      else
	{
	  fcache::line_info *i = NULL;
	  if (c->total_lines <= fcache_line_record_size)
	    {
	      /* In languages where the input file is not totally
		 preprocessed up front, the c->total_lines hint
		 can be smaller than the number of lines of the
		 file.  In that case, only the first
		 c->total_lines have been recorded.

		 Otherwise, the first c->total_lines we've read have
		 their start/end recorded here.  */
	      i = (line_num <= c->total_lines)
		? &c->line_record[line_num - 1]
		: &c->line_record[c->total_lines - 1];
	      gcc_assert (i->line_num <= line_num);
	    }
	  else
	    {
	      /*  So the file had more lines than our line record
		  size.  Thus the number of lines we've recorded has
		  been scaled down to fcache_line_reacord_size.  Let's
		  pick the start/end of the recorded line that is
		  closest to line_num.  */
	      size_t n = (line_num <= c->total_lines)
		? line_num * fcache_line_record_size / c->total_lines
		: c ->line_record.length () - 1;
	      if (n < c->line_record.length ())
		{
		  i = &c->line_record[n];
		  gcc_assert (i->line_num <= line_num);
		}
	    }

	  if (i && i->line_num == line_num)
	    {
	      /* We have the start/end of the line.  Let's just copy
		 it again and we are done.  */
	      ssize_t len = i->end_pos - i->start_pos + 1;
	      if (*line_len < len)
		*line = XRESIZEVEC (char, *line, len);
	      memmove (*line, c->data + i->start_pos, len);
	      (*line)[len - 1] = '\0';
	      *line_len = --len;
	      return true;
	    }

	  if (i)
	    {
	      c->line_start_idx = i->start_pos;
	      c->line_num = i->line_num - 1;
	    }
	  else
	    {
	      c->line_start_idx = 0;
	      c->line_num = 0;
	    }
	}
    }

  /*  Let's walk from line c->line_num up to line_num - 1, without
      copying any line.  */
  while (c->line_num < line_num - 1)
    if (!goto_next_line (c))
      return false;

  /* The line we want is the next one.  Let's read and copy it back to
     the caller.  */
  return read_next_line (c, line, line_len);
}

/* Return the physical source line that corresponds to FILE_PATH/LINE in a
   buffer that is statically allocated.  The newline is replaced by
   the null character.  Note that the line can contain several null
   characters, so LINE_LEN, if non-null, points to the actual length
   of the line.  */

const char *
location_get_source_line (const char *file_path, int line,
			  int *line_len)
{
  static char *buffer;
  static ssize_t len;

  if (line == 0)
    return NULL;

  fcache *c = lookup_or_add_file_to_cache_tab (file_path);
  if (c == NULL)
    return NULL;

  bool read = read_line_num (c, line, &buffer, &len);

  if (read && line_len)
    *line_len = len;

  return read ? buffer : NULL;
}

/* Test if the location originates from the spelling location of a
   builtin-tokens.  That is, return TRUE if LOC is a (possibly
   virtual) location of a built-in token that appears in the expansion
   list of a macro.  Please note that this function also works on
   tokens that result from built-in tokens.  For instance, the
   function would return true if passed a token "4" that is the result
   of the expansion of the built-in __LINE__ macro.  */
bool
is_location_from_builtin_token (source_location loc)
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
expand_location (source_location loc)
{
  return expand_location_1 (loc, /*expansion_point_p=*/true);
}

/* Expand the source location LOC into a human readable location.  If
   LOC is virtual, it resolves to the expansion location of the
   relevant macro.  If LOC resolves to a builtin location, the file
   name of the readable location is set to the string
   "<built-in>".  */

expanded_location
expand_location_to_spelling_point (source_location loc)
{
  return expand_location_1 (loc, /*expansion_point_p=*/false);
}

/* The rich_location class within libcpp requires a way to expand
   source_location instances, and relies on the client code
   providing a symbol named
     linemap_client_expand_location_to_spelling_point
   to do this.

   This is the implementation for libcommon.a (all host binaries),
   which simply calls into expand_location_to_spelling_point.  */

expanded_location
linemap_client_expand_location_to_spelling_point (source_location loc)
{
  return expand_location_to_spelling_point (loc);
}


/* If LOCATION is in a system header and if it is a virtual location for
   a token coming from the expansion of a macro, unwind it to the
   location of the expansion point of the macro.  Otherwise, just return
   LOCATION.

   This is used for instance when we want to emit diagnostics about a
   token that may be located in a macro that is itself defined in a
   system header, for example, for the NULL macro.  In such a case, if
   LOCATION were passed directly to diagnostic functions such as
   warning_at, the diagnostic would be suppressed (unless
   -Wsystem-headers).  */

source_location
expansion_point_location_if_in_system_header (source_location location)
{
  if (in_system_header_at (location))
    location = linemap_resolve_location (line_table, location,
					 LRK_MACRO_EXPANSION_POINT,
					 NULL);
  return location;
}

/* If LOCATION is a virtual location for a token coming from the expansion
   of a macro, unwind to the location of the expansion point of the macro.  */

source_location
expansion_point_location (source_location location)
{
  return linemap_resolve_location (line_table, location,
				   LRK_MACRO_EXPANSION_POINT, NULL);
}

/* Given location LOC, strip away any packed range information
   or ad-hoc information.  */

location_t
get_pure_location (location_t loc)
{
  if (IS_ADHOC_LOC (loc))
    loc
      = line_table->location_adhoc_data_map.data[loc & MAX_SOURCE_LOCATION].locus;

  if (loc >= LINEMAPS_MACRO_LOWEST_LOCATION (line_table))
    return loc;

  if (loc < RESERVED_LOCATION_COUNT)
    return loc;

  const line_map *map = linemap_lookup (line_table, loc);
  const line_map_ordinary *ordmap = linemap_check_ordinary (map);

  return loc & ~((1 << ordmap->m_range_bits) - 1);
}

/* Construct a location with caret at CARET, ranging from START to
   finish e.g.

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
  location_t pure_loc = get_pure_location (caret);
  source_range src_range;
  src_range.m_start = start;
  src_range.m_finish = finish;
  location_t combined_loc = COMBINE_LOCATION_DATA (line_table,
						   pure_loc,
						   src_range,
						   NULL);
  return combined_loc;
}

#define ONE_K 1024
#define ONE_M (ONE_K * ONE_K)

/* Display a number as an integer multiple of either:
   - 1024, if said integer is >= to 10 K (in base 2)
   - 1024 * 1024, if said integer is >= 10 M in (base 2)
 */
#define SCALE(x) ((unsigned long) ((x) < 10 * ONE_K \
		  ? (x) \
		  : ((x) < 10 * ONE_M \
		     ? (x) / ONE_K \
		     : (x) / ONE_M)))

/* For a given integer, display either:
   - the character 'k', if the number is higher than 10 K (in base 2)
     but strictly lower than 10 M (in base 2)
   - the character 'M' if the number is higher than 10 M (in base2)
   - the charcter ' ' if the number is strictly lower  than 10 K  */
#define STAT_LABEL(x) ((x) < 10 * ONE_K ? ' ' : ((x) < 10 * ONE_M ? 'k' : 'M'))

/* Display an integer amount as multiple of 1K or 1M (in base 2).
   Display the correct unit (either k, M, or ' ') after the amout, as
   well.  */
#define FORMAT_AMOUNT(size) SCALE (size), STAT_LABEL (size)

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
  fprintf (stderr, "Number of ordinary maps used:        %5ld%c\n",
           SCALE (s.num_ordinary_maps_used),
           STAT_LABEL (s.num_ordinary_maps_used));
  fprintf (stderr, "Ordinary map used size:              %5ld%c\n",
           SCALE (s.ordinary_maps_used_size),
           STAT_LABEL (s.ordinary_maps_used_size));
  fprintf (stderr, "Number of ordinary maps allocated:   %5ld%c\n",
           SCALE (s.num_ordinary_maps_allocated),
           STAT_LABEL (s.num_ordinary_maps_allocated));
  fprintf (stderr, "Ordinary maps allocated size:        %5ld%c\n",
           SCALE (s.ordinary_maps_allocated_size),
           STAT_LABEL (s.ordinary_maps_allocated_size));
  fprintf (stderr, "Number of macro maps used:           %5ld%c\n",
           SCALE (s.num_macro_maps_used),
           STAT_LABEL (s.num_macro_maps_used));
  fprintf (stderr, "Macro maps used size:                %5ld%c\n",
           SCALE (s.macro_maps_used_size),
           STAT_LABEL (s.macro_maps_used_size));
  fprintf (stderr, "Macro maps locations size:           %5ld%c\n",
           SCALE (s.macro_maps_locations_size),
           STAT_LABEL (s.macro_maps_locations_size));
  fprintf (stderr, "Macro maps size:                     %5ld%c\n",
           SCALE (macro_maps_size),
           STAT_LABEL (macro_maps_size));
  fprintf (stderr, "Duplicated maps locations size:      %5ld%c\n",
           SCALE (s.duplicated_macro_maps_locations_size),
           STAT_LABEL (s.duplicated_macro_maps_locations_size));
  fprintf (stderr, "Total allocated maps size:           %5ld%c\n",
           SCALE (total_allocated_map_size),
           STAT_LABEL (total_allocated_map_size));
  fprintf (stderr, "Total used maps size:                %5ld%c\n",
           SCALE (total_used_map_size),
           STAT_LABEL (total_used_map_size));
  fprintf (stderr, "Ad-hoc table size:                   %5ld%c\n",
	   SCALE (s.adhoc_table_size),
	   STAT_LABEL (s.adhoc_table_size));
  fprintf (stderr, "Ad-hoc table entries used:           %5ld\n",
	   s.adhoc_table_entries_used);
  fprintf (stderr, "optimized_ranges: %i\n",
	   line_table->num_optimized_ranges);
  fprintf (stderr, "unoptimized_ranges: %i\n",
	   line_table->num_unoptimized_ranges);

  fprintf (stderr, "\n");
}

/* Get location one beyond the final location in ordinary map IDX.  */

static source_location
get_end_location (struct line_maps *set, unsigned int idx)
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
		 source_location loc, int max_col, int divisor)
{
  fprintf (stream, "%*c", indent, ' ');
  fprintf (stream, "|");
  for (int column = 1; column < max_col; column++)
    {
      source_location column_loc = loc + (column << map->m_range_bits);
      write_digit (stream, column_loc / divisor);
    }
  fprintf (stream, "\n");
}

/* Write a half-closed (START) / half-open (END) interval of
   source_location to STREAM.  */

static void
dump_location_range (FILE *stream,
		     source_location start, source_location end)
{
  fprintf (stream,
	   "  source_location interval: %u <= loc < %u\n",
	   start, end);
}

/* Write a labelled description of a half-closed (START) / half-open (END)
   interval of source_location to STREAM.  */

static void
dump_labelled_location_range (FILE *stream,
			      const char *name,
			      source_location start, source_location end)
{
  fprintf (stream, "%s\n", name);
  dump_location_range (stream, start, end);
  fprintf (stream, "\n");
}

/* Write a visualization of the locations in the line_table to STREAM.  */

void
dump_location_info (FILE *stream)
{
  /* Visualize the reserved locations.  */
  dump_labelled_location_range (stream, "RESERVED LOCATIONS",
				0, RESERVED_LOCATION_COUNT);

  /* Visualize the ordinary line_map instances, rendering the sources. */
  for (unsigned int idx = 0; idx < LINEMAPS_ORDINARY_USED (line_table); idx++)
    {
      source_location end_location = get_end_location (line_table, idx);
      /* half-closed: doesn't include this one. */

      const line_map_ordinary *map
	= LINEMAPS_ORDINARY_MAP_AT (line_table, idx);
      fprintf (stream, "ORDINARY MAP: %i\n", idx);
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

      /* Render the span of source lines that this "map" covers.  */
      for (source_location loc = MAP_START_LOCATION (map);
	   loc < end_location;
	   loc += (1 << map->m_range_bits) )
	{
	  gcc_assert (pure_location_p (line_table, loc) );

	  expanded_location exploc
	    = linemap_expand_location (line_table, map, loc);

	  if (0 == exploc.column)
	    {
	      /* Beginning of a new source line: draw the line.  */

	      int line_size;
	      const char *line_text = location_get_source_line (exploc.file,
								exploc.line,
								&line_size);
	      if (!line_text)
		break;
	      fprintf (stream,
		       "%s:%3i|loc:%5i|%.*s\n",
		       exploc.file, exploc.line,
		       loc,
		       line_size, line_text);

	      /* "loc" is at column 0, which means "the whole line".
		 Render the locations *within* the line, by underlining
		 it, showing the source_location numeric values
		 at each column.  */
	      int max_col = (1 << map->m_column_and_range_bits) - 1;
	      if (max_col > line_size)
		max_col = line_size + 1;

	      int indent = 14 + strlen (exploc.file);

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
  for (unsigned int i = 0; i < LINEMAPS_MACRO_USED (line_table); i++)
    {
      /* Each macro map that is allocated owns source_location values
	 that are *lower* that the one before them.
	 Hence it's meaningful to view them either in order of ascending
	 source locations, or in order of ascending macro map index.  */
      const bool ascending_source_locations = true;
      unsigned int idx = (ascending_source_locations
			  ? (LINEMAPS_MACRO_USED (line_table) - (i + 1))
			  : i);
      const line_map_macro *map = LINEMAPS_MACRO_MAP_AT (line_table, idx);
      fprintf (stream, "MACRO %i: %s (%u tokens)\n",
	       idx,
	       linemap_map_get_macro_name (map),
	       MACRO_MAP_NUM_MACRO_TOKENS (map));
      dump_location_range (stream,
			   map->start_location,
			   (map->start_location
			    + MACRO_MAP_NUM_MACRO_TOKENS (map)));
      inform (MACRO_MAP_EXPANSION_POINT_LOCATION (map),
	      "expansion point is location %i",
	      MACRO_MAP_EXPANSION_POINT_LOCATION (map));
      fprintf (stream, "  map->start_location: %u\n",
	       map->start_location);

      fprintf (stream, "  macro_locations:\n");
      for (unsigned int i = 0; i < MACRO_MAP_NUM_MACRO_TOKENS (map); i++)
	{
	  source_location x = MACRO_MAP_LOCATIONS (map)[2 * i];
	  source_location y = MACRO_MAP_LOCATIONS (map)[(2 * i) + 1];

	  /* linemap_add_macro_token encodes token numbers in an expansion
	     by putting them after MAP_START_LOCATION. */

	  /* I'm typically seeing 4 uninitialized entries at the end of
	     0xafafafaf.
	     This appears to be due to macro.c:replace_args
	     adding 2 extra args for padding tokens; presumably there may
	     be a leading and/or trailing padding token injected,
	     each for 2 more location slots.
	     This would explain there being up to 4 source_locations slots
	     that may be uninitialized.  */

	  fprintf (stream, "    %u: %u, %u\n",
		   i,
		   x,
		   y);
	  if (x == y)
	    {
	      if (x < MAP_START_LOCATION (map))
		inform (x, "token %u has x-location == y-location == %u", i, x);
	      else
		fprintf (stream,
			 "x-location == y-location == %u encodes token # %u\n",
			 x, x - MAP_START_LOCATION (map));
		}
	  else
	    {
	      inform (x, "token %u has x-location == %u", i, x);
	      inform (x, "token %u has y-location == %u", i, y);
	    }
	}
      fprintf (stream, "\n");
    }

  /* It appears that MAX_SOURCE_LOCATION itself is never assigned to a
     macro map, presumably due to an off-by-one error somewhere
     between the logic in linemap_enter_macro and
     LINEMAPS_MACRO_LOWEST_LOCATION.  */
  dump_labelled_location_range (stream, "MAX_SOURCE_LOCATION",
				MAX_SOURCE_LOCATION,
				MAX_SOURCE_LOCATION + 1);

  /* Visualize ad-hoc values.  */
  dump_labelled_location_range (stream, "AD-HOC LOCATIONS",
				MAX_SOURCE_LOCATION + 1, UINT_MAX);
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

  string_concat *concat
    = new (ggc_alloc <string_concat> ()) string_concat (num, locs);
  m_table->put (key_loc, concat);
}

/* Determine if LOC was the location of the the initial token of a
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
			      string_concat_db *concats,
			      location_t strloc,
			      enum cpp_ttype type,
			      cpp_substring_ranges &ranges)
{
  gcc_assert (pfile);

  if (strloc == UNKNOWN_LOCATION)
    return "unknown location";

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
	/* If the string is within a macro expansion, we can't get at the
	   end location.  */
	return "macro expansion";

      if (src_range.m_start >= LINE_MAP_MAX_LOCATION_WITH_COLS)
	/* If so, we can't reliably determine where the token started within
	   its line.  */
	return "range starts after LINE_MAP_MAX_LOCATION_WITH_COLS";

      if (src_range.m_finish >= LINE_MAP_MAX_LOCATION_WITH_COLS)
	/* If so, we can't reliably determine where the token finished within
	   its line.  */
	return "range ends after LINE_MAP_MAX_LOCATION_WITH_COLS";

      expanded_location start
	= expand_location_to_spelling_point (src_range.m_start);
      expanded_location finish
	= expand_location_to_spelling_point (src_range.m_finish);
      if (start.file != finish.file)
	return "range endpoints are in different files";
      if (start.line != finish.line)
	return "range endpoints are on different lines";
      if (start.column > finish.column)
	return "range endpoints are reversed";

      int line_width;
      const char *line = location_get_source_line (start.file, start.line,
						   &line_width);
      if (line == NULL)
	return "unable to read source line";

      /* Determine the location of the literal (including quotes
	 and leading prefix chars, such as the 'u' in a u""
	 token).  */
      const char *literal = line + start.column - 1;
      int literal_length = finish.column - start.column + 1;

      gcc_assert (line_width >= (start.column - 1 + literal_length));
      cpp_string from;
      from.len = literal_length;
      /* Make a copy of the literal, to avoid having to rely on
	 the lifetime of the copy of the line within the cache.
	 This will be released by the auto_cpp_string_vec dtor.  */
      from.text = XDUPVEC (unsigned char, literal, literal_length);
      strs.safe_push (from);

      /* For very long lines, a new linemap could have started
	 halfway through the token.
	 Ensure that the loc_reader uses the linemap of the
	 *end* of the token for its start location.  */
      const line_map_ordinary *final_ord_map;
      linemap_resolve_location (line_table, src_range.m_finish,
				LRK_MACRO_EXPANSION_POINT, &final_ord_map);
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
get_source_location_for_substring (cpp_reader *pfile,
				   string_concat_db *concats,
				   location_t strloc,
				   enum cpp_ttype type,
				   int caret_idx, int start_idx, int end_idx,
				   source_location *out_loc)
{
  gcc_checking_assert (caret_idx >= 0);
  gcc_checking_assert (start_idx >= 0);
  gcc_checking_assert (end_idx >= 0);
  gcc_assert (out_loc);

  cpp_substring_ranges ranges;
  const char *err
    = get_substring_ranges_for_loc (pfile, concats, strloc, type, ranges);
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
    = get_substring_ranges_for_loc (pfile, concats, strloc, type, ranges);
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
				     string_concat_db *concats,
				     location_t strloc,
				     enum cpp_ttype type,
				     int *out)
{
  gcc_assert (out);

  cpp_substring_ranges ranges;
  const char *err
    = get_substring_ranges_for_loc (pfile, concats, strloc, type, ranges);

  if (err)
    return err;

  *out = ranges.get_num_ranges ();
  return NULL;
}

/* Selftests of location handling.  */

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
   - the fallback modes within line-map.c: there are various threshold
   values for source_location/location_t beyond line-map.c changes
   behavior (disabling of the range-packing optimization, disabling
   of column-tracking).  We can exercise these by starting the line_table
   at interesting values at or near these thresholds.

   The following struct describes a particular case within our test
   matrix.  */

struct line_table_case
{
  line_table_case (int default_range_bits, int base_location)
  : m_default_range_bits (default_range_bits),
    m_base_location (base_location)
  {}

  int m_default_range_bits;
  int m_base_location;
};

/* Constructor.  Store the old value of line_table, and create a new
   one, using sane defaults.  */

line_table_test::line_table_test ()
{
  gcc_assert (saved_line_table == NULL);
  saved_line_table = line_table;
  line_table = ggc_alloc<line_maps> ();
  linemap_init (line_table, BUILTINS_LOCATION);
  gcc_assert (saved_line_table->reallocator);
  line_table->reallocator = saved_line_table->reallocator;
  gcc_assert (saved_line_table->round_alloc_size);
  line_table->round_alloc_size = saved_line_table->round_alloc_size;
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
  gcc_assert (saved_line_table->reallocator);
  line_table->reallocator = saved_line_table->reallocator;
  gcc_assert (saved_line_table->round_alloc_size);
  line_table->round_alloc_size = saved_line_table->round_alloc_size;
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
  assert_loceq (_("<built-in>"), 0, 0, BUILTINS_LOCATION);
  ASSERT_PRED1 (is_location_from_builtin_token, BUILTINS_LOCATION);
}

/* Verify reading of input files (e.g. for caret-based diagnostics).  */

static void
test_reading_source_line ()
{
  /* Create a tempfile and write some text to it.  */
  temp_source_file tmp (SELFTEST_LOCATION, ".txt",
			"01234567890123456789\n"
			"This is the test text\n"
			"This is the 3rd line\n");

  /* Read back a specific line from the tempfile.  */
  int line_size;
  const char *source_line = location_get_source_line (tmp.get_filename (),
						      2, &line_size);
  ASSERT_TRUE (source_line != NULL);
  ASSERT_EQ (21, line_size);
  if (!strncmp ("This is the test text",
		source_line, line_size))
    ::selftest::pass (SELFTEST_LOCATION,
		      "source_line matched expected value");
  else
    ::selftest::fail (SELFTEST_LOCATION,
		      "source_line did not match expected value");

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

struct lexer_test;
class lexer_test_options;

/* A class for specifying options of a lexer_test.
   The "apply" vfunc is called during the lexer_test constructor.  */

class lexer_test_options
{
 public:
  virtual void apply (lexer_test &) = 0;
};

/* A struct for writing lexer tests.  */

struct lexer_test
{
  lexer_test (const line_table_case &case_, const char *content,
	      lexer_test_options *options);
  ~lexer_test ();

  const cpp_token *get_token ();

  temp_source_file m_tempfile;
  line_table_test m_ltt;
  cpp_reader *m_parser;
  string_concat_db m_concats;
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

  void apply (lexer_test &test) FINAL OVERRIDE
  {
    cpp_options *cpp_opts = cpp_get_options (test.m_parser);
    cpp_opts->narrow_charset = "IBM1047";

    cpp_callbacks *callbacks = cpp_get_callbacks (test.m_parser);
    callbacks->error = on_error;
  }

  static bool on_error (cpp_reader *pfile ATTRIBUTE_UNUSED,
			int level ATTRIBUTE_UNUSED,
			int reason ATTRIBUTE_UNUSED,
			rich_location *richloc ATTRIBUTE_UNUSED,
			const char *msgid, va_list *ap ATTRIBUTE_UNUSED)
    ATTRIBUTE_FPTR_PRINTF(5,0)
  {
    gcc_assert (s_singleton);
    /* Detect and record errors emitted by libcpp/charset.c:init_iconv_desc
       when the local iconv build doesn't support the conversion.  */
    if (strstr (msgid, "not supported by iconv"))
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

/* Constructor.  Override line_table with a new instance based on CASE_,
   and write CONTENT to a tempfile.  Create a cpp_reader, and use it to
   start parsing the tempfile.  */

lexer_test::lexer_test (const line_table_case &case_, const char *content,
			lexer_test_options *options) :
  /* Create a tempfile and write the text to it.  */
  m_tempfile (SELFTEST_LOCATION, ".c", content),
  m_ltt (case_),
  m_parser (cpp_create_reader (CLK_GNUC99, NULL, line_table)),
  m_concats ()
{
  if (options)
    options->apply (*this);

  cpp_init_iconv (m_parser);

  /* Parse the file.  */
  const char *fname = cpp_read_main_file (m_parser,
					  m_tempfile.get_filename ());
  ASSERT_NE (fname, NULL);
}

/* Destructor.  Verify that the next token in m_parser is EOF.  */

lexer_test::~lexer_test ()
{
  location_t loc;
  const cpp_token *tok;

  tok = cpp_get_token_with_location (m_parser, &loc);
  ASSERT_NE (tok, NULL);
  ASSERT_EQ (tok->type, CPP_EOF);

  cpp_finish (m_parser, NULL);
  cpp_destroy (m_parser);
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

  source_range actual_range;
  const char *err
    = get_source_range_for_char (pfile, concats, strloc, type, idx,
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
    = get_num_source_ranges_for_substring (pfile, concats, strloc, type,
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
    = get_substring_ranges_for_loc (pfile, concats, strloc,
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
     quotes.  */
  for (int i = 0; i <= 9; i++)
    ASSERT_CHAR_AT_RANGE (test, tok->src_loc, type, i, 1,
			  10 + i, 10 + i);

  ASSERT_NUM_SUBSTRING_RANGES (test, tok->src_loc, type, 10);
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
     quotes.  */
  for (int i = 0; i <= 4; i++)
    ASSERT_CHAR_AT_RANGE (test, tok->src_loc, type, i, 1, 10 + i, 10 + i);
  ASSERT_CHAR_AT_RANGE (test, tok->src_loc, type, 5, 1, 15, 18);
  for (int i = 6; i <= 9; i++)
    ASSERT_CHAR_AT_RANGE (test, tok->src_loc, type, i, 1, 13 + i, 13 + i);

  ASSERT_NUM_SUBSTRING_RANGES (test, tok->src_loc, type, 10);
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
     quotes.  */
  for (int i = 0; i < 5; i++)
    ASSERT_CHAR_AT_RANGE (test, tok->src_loc, type, i, 1, 10 + i, 10 + i);
  ASSERT_CHAR_AT_RANGE (test, tok->src_loc, type, 5, 1, 15, 18);
  for (int i = 6; i <= 9; i++)
    ASSERT_CHAR_AT_RANGE (test, tok->src_loc, type, i, 1, 13 + i, 13 + i);

  ASSERT_NUM_SUBSTRING_RANGES (test, tok->src_loc, type, 10);
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

  /* "bar".  */
  for (int i = 6; i <= 8; i++)
    ASSERT_CHAR_AT_RANGE (test, tok->src_loc, CPP_STRING,
			  i, 1, 13 + i, 13 + i);

  ASSERT_NUM_SUBSTRING_RANGES (test, tok->src_loc, CPP_STRING, 9);
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

  ASSERT_NUM_SUBSTRING_RANGES (test, tok->src_loc, CPP_STRING, 4);
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
     set.  Assuming that that is UTF-8, we should have the following:
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
     quotes.
     '01234'.  */
  for (int i = 0; i <= 4; i++)
    ASSERT_CHAR_AT_RANGE (test, tok->src_loc, type, i, 1, 10 + i, 10 + i);
  /* U+2174.  */
  for (int i = 5; i <= 7; i++)
    ASSERT_CHAR_AT_RANGE (test, tok->src_loc, type, i, 1, 15, 20);
  /* U+2175.  */
  for (int i = 8; i <= 10; i++)
    ASSERT_CHAR_AT_RANGE (test, tok->src_loc, type, i, 1, 21, 26);
  /* '789'.  */
  for (int i = 11; i <= 13; i++)
    ASSERT_CHAR_AT_RANGE (test, tok->src_loc, type, i, 1, 16 + i, 16 + i);

  ASSERT_NUM_SUBSTRING_RANGES (test, tok->src_loc, type, 14);
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
     quotes.
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

  ASSERT_NUM_SUBSTRING_RANGES (test, tok->src_loc, type, 14);
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
     quotes.  */
  for (int i = 0; i <= 9; i++)
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
     quotes.
     Assuming that both source and execution encodings are UTF-8, we have
     a run of 25 octets in each.  */
  for (int i = 0; i < 25; i++)
    ASSERT_CHAR_AT_RANGE (test, tok->src_loc, type, i, 1, 10 + i, 10 + i);

  ASSERT_NUM_SUBSTRING_RANGES (test, tok->src_loc, type, 25);
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

  /* Simulate c-lex.c's lex_string in order to record concatenation.  */
  test.m_concats.record_string_concatenation (2, input_locs);

  location_t initial_loc = input_locs[0];

  for (int i = 0; i <= 4; i++)
    ASSERT_CHAR_AT_RANGE (test, initial_loc, type, i, 1, 10 + i, 10 + i);
  for (int i = 5; i <= 9; i++)
    ASSERT_CHAR_AT_RANGE (test, initial_loc, type, i, 2, 5 + i, 5 + i);

  ASSERT_NUM_SUBSTRING_RANGES (test, initial_loc, type, 10);
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

  /* Simulate c-lex.c's lex_string in order to record concatenation.  */
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
	= get_source_range_for_char (test.m_parser, &test.m_concats,
				     initial_loc, type, 0, &actual_range);
      ASSERT_STREQ ("range starts after LINE_MAP_MAX_LOCATION_WITH_COLS", err);
      return;
    }

  for (int i = 0; i < 5; i++)
    for (int j = 0; j < 2; j++)
      ASSERT_CHAR_AT_RANGE (test, initial_loc, type, (i * 2) + j,
			    i + 1, 10 + j, 10 + j);

  ASSERT_NUM_SUBSTRING_RANGES (test, initial_loc, type, 10);
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

  /* Simulate c-lex.c's lex_string in order to record concatenation.  */
  test.m_concats.record_string_concatenation (4, input_locs);

  location_t initial_loc = input_locs[0];

  for (int i = 0; i <= 4; i++)
    ASSERT_CHAR_AT_RANGE (test, initial_loc, type, i, 1, 10 + i, 10 + i);
  ASSERT_CHAR_AT_RANGE (test, initial_loc, type, 5, 1, 19, 22);
  ASSERT_CHAR_AT_RANGE (test, initial_loc, type, 6, 1, 27, 30);
  for (int i = 7; i <= 9; i++)
    ASSERT_CHAR_AT_RANGE (test, initial_loc, type, i, 1, 28 + i, 28 + i);

  ASSERT_NUM_SUBSTRING_RANGES (test, initial_loc, type, 10);
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
  for (int i = 0; i <= 9; i++)
    ASSERT_CHAR_AT_RANGE (test, tok->src_loc, CPP_STRING,
			  i, 1, 20 + i, 20 + i);

  ASSERT_NUM_SUBSTRING_RANGES (test, tok->src_loc, CPP_STRING, 10);

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

   when c-format.c erroneously used the indicated one-character
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
  ASSERT_NUM_SUBSTRING_RANGES (test, tok->src_loc, CPP_STRING, 130);
  for (int i = 0; i < 130; i++)
    ASSERT_CHAR_AT_RANGE (test, tok->src_loc, CPP_STRING,
			  i, 2, 7 + i, 7 + i);
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
  LINE_MAP_MAX_LOCATION_WITH_COLS - 0x100,
  LINE_MAP_MAX_LOCATION_WITH_COLS - 1,
  LINE_MAP_MAX_LOCATION_WITH_COLS,
  LINE_MAP_MAX_LOCATION_WITH_COLS + 1,
  LINE_MAP_MAX_LOCATION_WITH_COLS + 0x100,
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
     (b) line_table->default_range_bits == 5.  */
  int num_cases_tested = 0;
  for (int default_range_bits = 0; default_range_bits <= 5;
       default_range_bits += 5)
    {
      /* ...and use each of the "interesting" location values as
	 the starting location within line_table.  */
      const int num_boundary_locations
	= sizeof (boundary_locations) / sizeof (boundary_locations[0]);
      for (int loc_idx = 0; loc_idx < num_boundary_locations; loc_idx++)
	{
	  line_table_case c (default_range_bits, boundary_locations[loc_idx]);

	  testcase (c);

	  num_cases_tested++;
	}
    }

  /* Verify that we fully covered the test matrix.  */
  ASSERT_EQ (num_cases_tested, 2 * 12);
}

/* Run all of the selftests within this file.  */

void
input_c_tests ()
{
  test_should_have_column_data_p ();
  test_unknown_location ();
  test_builtins ();

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
  for_each_line_table_case (test_lexer_char_constants);

  test_reading_source_line ();
}

} // namespace selftest

#endif /* CHECKING_P */
