/* Input handling for G++.
   Copyright (C) 1992, 93-98, 1999 Free Software Foundation, Inc.
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
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* G++ needs to do enough saving and re-parsing of text that it is
   necessary to abandon the simple FILE* model and use a mechanism where
   we can pre-empt one input stream with another derived from saved text;
   we may need to do this arbitrarily often, and cannot depend on having
   the GNU library available, so FILE objects just don't cut it.

   This file is written as a separate module, but can be included by
   lex.c for very minor efficiency gains (primarily in function
   inlining).  */

#include "system.h"

extern FILE *finput;

struct input_source {
  /* saved string */
  char *str;
  int length;
  /* current position, when reading as input */
  int offset;
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

#if USE_CPPLIB
extern unsigned char *yy_cur, *yy_lim;
extern int yy_get_token ();
#define GETC() (yy_cur < yy_lim ? *yy_cur++ : yy_get_token ())
#else
#define GETC() getc (finput)
#endif

extern void feed_input PROTO((char *, int));
extern void put_input PROTO((int));
extern void put_back PROTO((int));
extern int getch PROTO((void));
extern int input_redirected PROTO((void));

static inline struct input_source * allocate_input PROTO((void));
static inline void free_input PROTO((struct input_source *));
static inline void end_input PROTO((void));
static inline int sub_getch PROTO((void));

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
  return inp;
}

static inline void
free_input (inp)
     struct input_source *inp;
{
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
feed_input (str, len)
     char *str;
     int len;
{
  struct input_source *inp = allocate_input ();

  /* This shouldn't be necessary.  */
  while (len && !str[len-1])
    len--;

  inp->str = str;
  inp->length = len;
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

static inline void
end_input ()
{
  struct input_source *inp = input;

  end_of_file = 0;
  input = inp->next;
  input_filename = inp->filename;
  lineno = inp->lineno;
  /* Get interface/implementation back in sync.  */
  extract_interface_info ();
  putback_char = inp->putback_char;
  restore_pending_input (inp->input);
  free_input (inp);
}

static inline int
sub_getch ()
{
  if (putback_char != -1)
    {
      int ch = putback_char;
      putback_char = -1;
      return ch;
    }
  if (input)
    {
      if (input->offset >= input->length)
	{
	  my_friendly_assert (putback_char == -1, 223);
	  ++(input->offset);
	  if (input->offset - input->length < 64)
	    return EOF;

	  /* We must be stuck in an error-handling rule; give up.  */
	  end_input ();
	  return getch ();
	}
      return (unsigned char)input->str[input->offset++];
    }
  return GETC ();
}

inline
void
put_back (ch)
     int ch;
{
  if (ch != EOF)
    {
      my_friendly_assert (putback_char == -1, 224);
      putback_char = ch;
    }
}

extern int linemode;

int
getch ()
{
  int ch = sub_getch ();
  if (linemode && ch == '\n')
    {
      put_back (ch);
      ch = EOF;
    }
  return ch;
}

inline
int
input_redirected ()
{
  return input != 0;
}
