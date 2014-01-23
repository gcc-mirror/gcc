/* Data and functions related to line maps and input files.
   Copyright (C) 2004-2014 Free Software Foundation, Inc.

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
#include "input.h"
#include "vec.h"

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

location_t input_location;

struct line_maps *line_table;

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
  const struct line_map *map;
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
							  loc, &map);
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

/* Free the ressources used by the set of cache used for files accessed
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
  if (ferror (fp))
    {
      fclose (fp);
      return NULL;
    }

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

/* Return the physical source line that corresponds to xloc in a
   buffer that is statically allocated.  The newline is replaced by
   the null character.  Note that the line can contain several null
   characters, so LINE_LEN, if non-null, points to the actual length
   of the line.  */

const char *
location_get_source_line (expanded_location xloc,
			  int *line_len)
{
  static char *buffer;
  static ssize_t len;

  fcache * c = lookup_or_add_file_to_cache_tab (xloc.file);
  bool read = read_line_num (c, xloc.line, &buffer, &len);

  if (read && line_len)
    *line_len = len;

  return read ? buffer : NULL;
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
  return expand_location_1 (loc, /*expansion_piont_p=*/false);
}

/* If LOCATION is in a system header and if it's a virtual location for
   a token coming from the expansion of a macro M, unwind it to the
   location of the expansion point of M.  Otherwise, just return
   LOCATION.

   This is used for instance when we want to emit diagnostics about a
   token that is located in a macro that is itself defined in a system
   header -- e.g for the NULL macro.  In that case, if LOCATION is
   passed to diagnostics emitting functions like warning_at as is, no
   diagnostic won't be emitted.  */

source_location
expansion_point_location_if_in_system_header (source_location location)
{
  if (in_system_header_at (location))
    location = linemap_resolve_location (line_table, location,
					 LRK_MACRO_EXPANSION_POINT,
					 NULL);
  return location;
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
  fprintf (stderr, "\n");
}
