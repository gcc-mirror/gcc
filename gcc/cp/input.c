/* Input handling for G++.
   Copyright (C) 1992, 1993 Free Software Foundation, Inc.
   Written by Ken Raeburn (raeburn@cygnus.com) while at Watchmaker Computing.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* G++ needs to do enough saving and re-parsing of text that it is
   necessary to abandon the simple FILE* model and use a mechanism where
   we can pre-empt one input stream with another derived from saved text;
   we may need to do this arbitrarily often, and cannot depend on having
   the GNU library available, so FILE objects just don't cut it.

   This file is written as a separate module, but can be included by
   lex.c for very minor efficiency gains (primarily in function
   inlining).  */

#include <stdio.h>
#include "obstack.h"

extern FILE *finput;

struct pending_input *save_pending_input ();
void restore_pending_input ();

struct input_source {
  /* saved string */
  char *str;
  int length;
  /* current position, when reading as input */
  int offset;
  /* obstack to free this input string from when finished, if any */
  struct obstack *obstack;
  /* linked list maintenance */
  struct input_source *next;
  /* values to restore after reading all of current string */
  char *filename;
  int lineno;
  struct pending_input *input;
  int putback_char;
};

static struct input_source *input, *free_inputs;

extern char *input_filename;
extern int lineno;

#ifdef __GNUC__
#define inline __inline__
#else
#define inline
#endif

static inline struct input_source *
allocate_input ()
{
  struct input_source *inp;
  if (free_inputs)
    {
      inp = free_inputs;
      free_inputs = inp->next;
      inp->next = 0;
      return inp;
    }
  inp = (struct input_source *) xmalloc (sizeof (struct input_source));
  inp->next = 0;
  inp->obstack = 0;
  return inp;
}

static inline void
free_input (inp)
     struct input_source *inp;
{
  if (inp->obstack)
    obstack_free (inp->obstack, inp->str);
  inp->obstack = 0;
  inp->str = 0;
  inp->length = 0;
  inp->next = free_inputs;
  free_inputs = inp;
}

static int putback_char = -1;

/* Some of these external functions are declared inline in case this file
   is included in lex.c.  */

inline
void
feed_input (str, len, delete)
     char *str;
     int len;
     struct obstack *delete;
{
  struct input_source *inp = allocate_input ();

  /* This shouldn't be necessary.  */
  while (len && !str[len-1])
    len--;

  inp->str = str;
  inp->length = len;
  inp->obstack = delete;
  inp->offset = 0;
  inp->next = input;
  inp->filename = input_filename;
  inp->lineno = lineno;
  inp->input = save_pending_input ();
  inp->putback_char = putback_char;
  putback_char = -1;
  input = inp;
}

struct pending_input *to_be_restored; /* XXX */
extern int end_of_file;

int
getch ()
{
  if (putback_char != -1)
    {
      int ch = putback_char;
      putback_char = -1;
      return ch;
    }
  if (input)
    {
      if (input->offset == input->length)
	{
	  struct input_source *inp = input;
	  my_friendly_assert (putback_char == -1, 223);
	  to_be_restored = inp->input;
	  input->offset++;
	  return EOF;
	}
      else if (input->offset > input->length)
	{
	  struct input_source *inp = input;

	  end_of_file = 0;
	  input = inp->next;
	  input_filename = inp->filename;
	  lineno = inp->lineno;
	  /* Get interface/implementation back in sync. */
	  extract_interface_info ();
	  putback_char = inp->putback_char;
	  free_input (inp);
	  return getch ();
	}
      if (input)
	return input->str[input->offset++];
    }
  return getc (finput);
}

inline
void
put_back (ch)
     int ch;
{
  my_friendly_assert (putback_char == -1, 224);
  putback_char = ch;
}

inline
int
input_redirected ()
{
  return input != 0;
}
