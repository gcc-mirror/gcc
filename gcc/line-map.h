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

#ifndef GCC_LINE_MAP_H
#define GCC_LINE_MAP_H

/* The logical line FROM_LINE maps to physical source file TO_FILE at
   line TO_LINE, and subsequently one-to-one until the next line_map
   structure in the set.  */
struct line_map
{
  const char *to_file;
  unsigned int to_line;
  unsigned int from_line;
  int included_from;
};

/* Contains a sequence of chronological line_map structures.  */
struct line_maps
{
  struct line_map *maps;
  unsigned int allocated;
  unsigned int used;
};

/* Reason for adding a line change with add_line_map ().  */
enum lc_reason {LC_ENTER = 0, LC_LEAVE, LC_RENAME};

/* Initialize a line map set.  */
extern void init_line_maps
  PARAMS ((struct line_maps *));

/* Free a line map set.  */
extern void free_line_maps
  PARAMS ((struct line_maps *));

/* Add a mapping of logical source line to physical source file and
   line number.  Ther text pointed to by TO_FILE must have a lifetime
   at least as long as the final call to lookup_line ().

   FROM_LINE should be monotonic increasing across calls to this
   function.  */
extern struct line_map *add_line_map
  PARAMS ((struct line_maps *, enum lc_reason,
	   unsigned int from_line, const char *to_file, unsigned int to_line));

/* Given a logical line, returns the map from which the corresponding
   (source file, line) pair can be deduced.  */
extern struct line_map *lookup_line
  PARAMS ((struct line_maps *, unsigned int));

/* Converts a map and logical line to source line.  */
#define SOURCE_LINE(MAP, LINE) ((LINE) + (MAP)->to_line - (MAP)->from_line)

/* Returns the last source line within a map.  This is the (last) line
   of the #include, or other directive, that caused a map change.  */
#define LAST_SOURCE_LINE(MAP) SOURCE_LINE (MAP, (MAP)[1].from_line - 1)

#define MAIN_FILE_P(MAP) ((MAP)->included_from < 0)

#endif /* !GCC_LINE_MAP_H  */
