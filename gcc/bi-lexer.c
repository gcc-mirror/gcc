/* Lexer for scanner of bytecode definition file.
   Copyright (C) 1993, 1995 Free Software Foundation, Inc.

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

#include <stdio.h>
#include "hconfig.h"
#include "bi-parser.h"


/* Safely allocate NBYTES bytes of memory.  Returns pointer to block of
   memory. */

static char *
xmalloc (nbytes)
     int nbytes;
{
  char *tmp = (char *) malloc (nbytes);

  if (!tmp)
    {
      fprintf (stderr, "can't allocate %d bytes (out of virtual memory)\n", nbytes);
      exit (FATAL_EXIT_CODE);
    }

  return tmp;
}


/* Safely reallocate BLOCK so its size becomes NBYTES.
   The block returned may be different from the one supplied. */

static char *
xrealloc (block, nbytes)
     char *block;
     int nbytes;
{
  char *tmp = (block
	       ? (char *) realloc (block, nbytes)
	       : (char *) malloc (nbytes));

  if (!tmp)
    {
      fprintf (stderr, "can't reallocate %d bytes (out of virtual memory)\n", nbytes);
      exit (FATAL_EXIT_CODE);
    }

  return tmp;
}


/* Scan for string token on standard input.  A string is, for our
   purposes here, a sequence of characters that starts with the regexp
   ``[^ #\t\n(),]'' and is then followed by the regexp ``[^#(),]*''. Any
   character is accepted if preceded by a backslash, "\\".  It is assumed
   that the first character has already been checked by the main loop. */

static char *
scan_string ()
{
  char *buffer = NULL;
  char *point = NULL;
  int buffer_size = 0;
  int c;

  while ((c = getc (stdin)) != EOF
	 && c != '#' && c != '(' && c != ')' && c != ',')
    {
      /* Extend buffer, if necessary (minus two so there's room for the NUL
	 trailer as well as another character if this one is a backslash).  */
      if (!buffer_size || (point - buffer >= buffer_size-2))
	{
	  int previous_point_index = point - buffer;

	  buffer_size = (!buffer_size ? 32 : buffer_size * 2);
	  if (!buffer)
	    buffer = xmalloc (buffer_size);
	  else
	    buffer = xrealloc (buffer, buffer_size);
	  
	  point = buffer + previous_point_index;
	}
      *point++ = c & 0xff;

      if (c == '\\')
	{
	  c = getc (stdin);

	  /* Catch special case: backslash at end of file */
	  if (c == EOF)
	    break;

	  *point++ = c;
	}
    }
  *point = 0;

  if (c != EOF)
    ungetc (c, stdin);

  return buffer;
}


int
yylex ()
{
  int c;
  char *token;


  /* First char determines what token we're looking at */
  for (;;)
    {
      c = getc (stdin);

      switch (c)
	{
	case EOF:
	  return 0;
	  
	case ' ':
	case '\t':
	case '\n':
	  /* Ignore whitespace */
	  continue;
	  
	case '#':
	  /* Comments advance to next line */
	  while ((c = getc (stdin)) != '\n' && c != EOF);
	  continue;
	  
	default:
	  if (c != '(' && c != ')' && c != '\\' && c != ',')
	    {
	      ungetc (c, stdin);
	      yylval.string = scan_string ();

	      /* Check if string is "define_operator"; if so, return
		 a DEFOP token instead.  */
	      if (!strcmp (yylval.string, "define_operator"))
		{
		  free (yylval.string);
		  yylval.string = 0;
		  return DEFOP;
		}
	      return STRING;
	    }
	  return c & 0xff;
	}
    }
}
