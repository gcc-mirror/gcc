/* Input handling for G++.
   Copyright (C) 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000
   Free Software Foundation, Inc.
   Written by Ken Raeburn (raeburn@cygnus.com) while at Watchmaker Computing.
   Enhanced by Michael Tiemann (tiemann@cygnus.com) to better support USE_CPPLIB

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

#if !USE_CPPLIB
struct putback_buffer {
  char *buffer;
  int   buffer_size;
  int   index;
};

static struct putback_buffer putback = {NULL, 0, -1};
#endif

struct input_source {
  /* saved string */
  char *str;
  int length;
  /* current position, when reading as input */
  int offset;
  /* linked list maintenance */
  struct input_source *next;
  /* values to restore after reading all of current string */
  struct pending_input *input;
#if !USE_CPPLIB
  char *filename;
  int lineno;
  struct putback_buffer putback;
#endif
};

static struct input_source *input, *free_inputs;

extern char *input_filename;
extern int lineno;

#if USE_CPPLIB
extern unsigned char *yy_cur, *yy_lim;
extern int yy_get_token ();
#endif

extern void feed_input PARAMS ((char *, int, char *, int));
extern void put_input PARAMS ((int));
extern void put_back PARAMS ((int));
extern int getch PARAMS ((void));
extern int input_redirected PARAMS ((void));

static inline struct input_source * allocate_input PARAMS ((void));
static inline void free_input PARAMS ((struct input_source *));
static inline void end_input PARAMS ((void));

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

/* Some of these external functions are declared inline in case this file
   is included in lex.c.  */

inline
void
feed_input (str, len, file, line)
     char *str;
     int len;
     char *file;
     int line;
{
  struct input_source *inp = allocate_input ();

  /* This shouldn't be necessary.  */
  while (len && !str[len-1])
    len--;

#if USE_CPPLIB
  if (yy_lim > yy_cur)
    /* If we've started reading the next token, we're hosed.  The
       token_getch stuff is supposed to prevent this from happening.  */
    my_friendly_abort (990710);
  cpp_push_buffer (&parse_in, str, len);
  CPP_BUFFER (&parse_in)->manual_pop = 1;
  CPP_BUFFER (&parse_in)->nominal_fname
    = CPP_BUFFER (&parse_in)->fname = file;
  CPP_BUFFER (&parse_in)->lineno = parse_in.lineno = line;
#else
  inp->str = str;
  inp->length = len;
  inp->offset = 0;
  inp->putback = putback;
  inp->filename = input_filename;
  inp->lineno = lineno;
  putback.buffer = NULL;
  putback.buffer_size = 0;
  putback.index = -1;
#endif
  inp->next = input;
  inp->input = save_pending_input ();
  input = inp;
  lineno = line;
  input_filename = file;
}

extern int end_of_file;

static inline void
end_input ()
{
  struct input_source *inp = input;

#if USE_CPPLIB
  cpp_pop_buffer (&parse_in);
#else
  putback = inp->putback;
  input_filename = inp->filename;
  lineno = inp->lineno;
#endif

  end_of_file = 0;
  input = inp->next;
  /* Get interface/implementation back in sync.  */
  extract_interface_info ();
  restore_pending_input (inp->input);
  free_input (inp);
}

inline int
getch ()
{
#if USE_CPPLIB
  return (yy_cur < yy_lim ? *yy_cur++ : yy_get_token ());
#else
  if (putback.index != -1)
    {
      int ch = putback.buffer[putback.index];
      --putback.index;
      return ch;
    }
  if (input)
    {
      if (input->offset >= input->length)
	{
	  my_friendly_assert (putback.index == -1, 223);
	  ++(input->offset);
	  if (input->offset - input->length < 64)
	    return EOF;

	  /* We must be stuck in an error-handling rule; give up.  */
	  end_input ();
	  return getch ();
	}
      return (unsigned char)input->str[input->offset++];
    }
  return getc (finput);
#endif
}

inline
void
put_back (ch)
     int ch;
{
#if USE_CPPLIB
  if (ch == EOF)
    ;
  else if (yy_cur[-1] != ch)
    my_friendly_abort (990709);
  else
    yy_cur--;
#else
  if (ch != EOF)
    {
      if (putback.index == putback.buffer_size - 1)
	{
	  putback.buffer_size += 16;
	  putback.buffer = xrealloc (putback.buffer, putback.buffer_size);
	}
      my_friendly_assert (putback.buffer != NULL, 224);
      putback.buffer[++putback.index] = ch;
    }
#endif
}

inline
int
input_redirected ()
{
#ifdef USE_CPPLIB
  return CPP_BUFFER(&parse_in)->manual_pop;
#else
  return input != 0;
#endif
}
