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

/* Reason for adding a line change with add_line_map ().  LC_ENTER is
   when including a new file, e.g. a #include directive in C.
   LC_LEAVE is when reaching a file's end.  LC_RENAME is when a file
   name or line number changes for neither of the above reasons
   (e.g. a #line directive in C).  */
enum lc_reason {LC_ENTER = 0, LC_LEAVE, LC_RENAME};

/* The logical line FROM_LINE maps to physical source file TO_FILE at
   line TO_LINE, and subsequently one-to-one until the next line_map
   structure in the set.  INCLUDED_FROM is an index into the set that
   gives the line mapping at whose end the current one was included.
   File(s) at the bottom of the include stack have this set to -1.
   REASON is the reason for creation of this line map, SYSP is one for
   a system header, two for a C system header file that therefore
   needs to be extern "C" protected in C++, and zero otherwise.  */
struct line_map
{
  const char *to_file;
  unsigned int to_line;
  unsigned int from_line;
  int included_from;
  ENUM_BITFIELD (lc_reason) reason : CHAR_BIT;
  unsigned char sysp;
};

/* A set of chronological line_map structures.  */
struct line_maps
{
  struct line_map *maps;
  unsigned int allocated;
  unsigned int used;

  /* The most recently listed include stack, if any, starts with
     LAST_LISTED as the topmost including file.  -1 indicates nothing
     has been listed yet.  */
  int last_listed;

  /* Depth of the include stack, including the current file.  */
  unsigned int depth;

  /* If true, prints an include trace a la -H.  */
  bool trace_includes;
};

/* Initialize a line map set.  */
extern void init_line_maps
  PARAMS ((struct line_maps *));

/* Free a line map set.  */
extern void free_line_maps
  PARAMS ((struct line_maps *));

/* Add a mapping of logical source line to physical source file and
   line number.  The text pointed to by TO_FILE must have a lifetime
   at least as long as the line maps.  If reason is LC_LEAVE, and
   TO_FILE is NULL, then TO_FILE, TO_LINE and SYSP are given their
   natural values considering the file we are returning to.

   FROM_LINE should be monotonic increasing across calls to this
   function.  A call to this function can relocate the previous set of
   maps, so any stored line_map pointers should not be used.  */
extern const struct line_map *add_line_map
  PARAMS ((struct line_maps *, enum lc_reason, unsigned int sysp,
	   unsigned int from_line, const char *to_file, unsigned int to_line));

/* Given a logical line, returns the map from which the corresponding
   (source file, line) pair can be deduced.  */
extern const struct line_map *lookup_line
  PARAMS ((struct line_maps *, unsigned int));

/* Print the file names and line numbers of the #include commands
   which led to the map MAP, if any, to stderr.  Nothing is output if
   the most recently listed stack is the same as the current one.  */
extern void print_containing_files
  PARAMS ((struct line_maps *, const struct line_map *));

/* Converts a map and logical line to source line.  */
#define SOURCE_LINE(MAP, LINE) ((LINE) + (MAP)->to_line - (MAP)->from_line)

/* Returns the last source line within a map.  This is the (last) line
   of the #include, or other directive, that caused a map change.  */
#define LAST_SOURCE_LINE(MAP) SOURCE_LINE ((MAP), (MAP)[1].from_line - 1)

/* Returns the map a given map was included from.  */
#define INCLUDED_FROM(SET, MAP) (&(SET)->maps[(MAP)->included_from])

/* Nonzero if the map is at the bottom of the include stack.  */
#define MAIN_FILE_P(MAP) ((MAP)->included_from < 0)

/* The current line map.  Saves a call to lookup_line if the caller is
   sure he is in the scope of the current map.  */
#define CURRENT_LINE_MAP(MAPS) ((MAPS)->maps + (MAPS)->used - 1)

#endif /* !GCC_LINE_MAP_H  */
