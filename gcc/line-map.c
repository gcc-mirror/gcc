/* Map logical line numbers to (source file, line number) pairs.
   Copyright (C) 2001
   Free Software Foundation, Inc.

This program is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

 In other words, you are welcome to use, share and improve this program.
 You are forbidden to forbid anyone else to use, share and improve
 what you give them.   Help stamp out software-hoarding!  */

#include "config.h"
#include "system.h"
#include "line-map.h"
#include "intl.h"

/* Initialize a line map set.  */

void
init_line_maps (set)
     struct line_maps *set;
{
  set->maps = 0;
  set->allocated = 0;
  set->used = 0;
  set->last_listed = -1;
}

/* Free a line map set.  */

void
free_line_maps (set)
     struct line_maps *set;
{
  if (set->maps)
    {
#ifdef ENABLE_CHECKING
      struct line_map *map;

      for (map = CURRENT_LINE_MAP (set); ! MAIN_FILE_P (map);
	   map = INCLUDED_FROM (set, map))
	fprintf (stderr, "line-map.c: file \"%s\" entered but not left\n",
		 map->to_file);
#endif
      free (set->maps);
    }
}

/* Add a mapping of logical source line to physical source file and
   line number.  Ther text pointed to by TO_FILE must have a lifetime
   at least as long as the final call to lookup_line ().

   FROM_LINE should be monotonic increasing across calls to this
   function.  */

struct line_map *
add_line_map (set, reason, from_line, to_file, to_line)
     struct line_maps *set;
     enum lc_reason reason;
     unsigned int from_line;
     const char *to_file;
     unsigned int to_line;
{
  struct line_map *map;

  if (set->used && from_line < set->maps[set->used - 1].from_line)
    abort ();

  if (set->used == set->allocated)
    {
      set->allocated = 2 * set->allocated + 256;
      set->maps = (struct line_map *)
	xrealloc (set->maps, set->allocated * sizeof (struct line_map));
    }

  map = &set->maps[set->used];
  map->from_line = from_line;
  map->to_file = to_file;
  map->to_line = to_line;

  /* If we don't keep our line maps consistent, we can easily
     segfault.  Don't rely on the client to do it for us.  */
  if (set->used == 0)
    reason = LC_ENTER;
  else if (reason == LC_LEAVE)
    {
      if (MAIN_FILE_P (map - 1)
	  || strcmp (INCLUDED_FROM (set, map - 1)->to_file, to_file))
	{
#ifdef ENABLE_CHECKING
	  fprintf (stderr, "line-map.c: file \"%s\" left but not entered\n",
		   to_file);
#endif
	  reason = LC_RENAME;
	}
    }

  if (reason == LC_ENTER)
    map->included_from = set->used - 1;
  else if (reason == LC_RENAME)
    map->included_from = map[-1].included_from;
  else if (reason == LC_LEAVE)
    map->included_from = INCLUDED_FROM (set, map - 1)->included_from;

  set->used++;
  return map;
}

/* Given a logical line, returns the map from which the corresponding
   (source file, line) pair can be deduced.  Since the set is built
   chronologically, the logical lines are monotonic increasing, and so
   the list is sorted and we can use a binary search.  */

struct line_map *
lookup_line (set, line)
     struct line_maps *set;
     unsigned int line;
{
  unsigned int md, mn = 0, mx = set->used;

  if (mx == 0)
    abort ();

  while (mx - mn > 1)
    {
      md = (mn + mx) / 2;
      if (set->maps[md].from_line > line)
	mx = md;
      else
	mn = md;
    }

  return &set->maps[mn];
}

/* Print the file names and line numbers of the #include commands
   which led to the map MAP, if any, to stderr.  Nothing is output if
   the most recently listed stack is the same as the current one.  */

void
print_containing_files (set, map)
     struct line_maps *set;
     struct line_map *map;
{
  if (MAIN_FILE_P (map) || set->last_listed == map->included_from)
    return;

  set->last_listed = map->included_from;
  map = INCLUDED_FROM (set, map);

  fprintf (stderr,  _("In file included from %s:%u"),
	   map->to_file, LAST_SOURCE_LINE (map));

  while (! MAIN_FILE_P (map))
    {
      map = INCLUDED_FROM (set, map);
      /* Translators note: this message is used in conjunction
	 with "In file included from %s:%ld" and some other
	 tricks.  We want something like this:

	 | In file included from sys/select.h:123,
	 |                  from sys/types.h:234,
	 |                  from userfile.c:31:
	 | bits/select.h:45: <error message here>

	 with all the "from"s lined up.
	 The trailing comma is at the beginning of this message,
	 and the trailing colon is not translated.  */
      fprintf (stderr, _(",\n                 from %s:%u"),
	       map->to_file, LAST_SOURCE_LINE (map));
    }

  fputs (":\n", stderr);
}
