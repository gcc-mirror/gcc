/* scan-decls.c - Extracts declarations from cpp output.
   Copyright (C) 1993 Free Software Foundation, Inc.

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
Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* This filter scans a C source file (actually, the output of cpp).
   It looks for function declaration.  For each declaration, it prints:

   	NAME;C;RTYPE;ARGS;FILENAME;LINENO;

   NAME is the function's name.
   C is "F" if the declaration is nested inside 'extern "C"' braces;
   otherwise "f".
   RTYPE is the function's return type.
   ARGS is the function's argument list.
   FILENAME and LINENO is where the declarations was seen
   (taking #-directives into account).

   Also:

	NAME;M;
   indicates that the macro NAME was seen (when invoked from fixproto).
	NAME;X;TYPE;
   indicates that 'extern TYPE NAME;' was seen.

   Written by Per Bothner <bothner@cygnus.com>, July 1993.
   */

#include <stdio.h>
#include <ctype.h>
#include "scan.h"

sstring buf;
sstring rtype;

int brace_nesting = 0;

/* The first extern_C_braces_length elements of extern_C_braces
   indicate the (brace nesting levels of) left braces that were
   prefixed by extern "C". */
int extern_C_braces_length = 0;
char extern_C_braces[20];
#define in_extern_C_brace (extern_C_braces_length>0)

/* True if the function declaration currently being scanned is
   prefixed by extern "C". */
int current_extern_C = 0;

int
main ()
{
  FILE *fp = stdin;
  int c;
  int saw_extern;

 new_statement:
  c = get_token (fp, &buf);
 handle_statement:
  current_extern_C = 0;
  saw_extern = 0;
  if (c == '}')
    {
      /* pop an 'extern "C"' nesting level, if appropriate */
      if (extern_C_braces_length
	  && extern_C_braces[extern_C_braces_length - 1] == brace_nesting)
	extern_C_braces_length--;
      brace_nesting--;
      goto new_statement;
    }
  if (c == '{')
    {
      brace_nesting++;
      goto new_statement;
    }
  if (c == EOF)
    return 0;
  if (c == ';')
    goto new_statement;
  if (c != IDENTIFIER_TOKEN)
    goto new_statement;
  rtype.ptr = rtype.base;
  if (SSTRING_LENGTH (&buf) > 16
      && strncmp (buf.base, "__DEFINED_MACRO_", 16) == 0)
    {
      fprintf (stdout, "%s;M;\n", buf.base+16);
      goto new_statement;
    }
  if (strcmp (buf.base, "extern") == 0)
    {
      saw_extern = 1;
      c = get_token (fp, &buf);
      if (c == STRING_TOKEN && strcmp (buf.base, "C") == 0)
	{
	  current_extern_C = 1;
	  c = get_token (fp, &buf);
	  if (c == '{')
	    {
	      brace_nesting++;
	      extern_C_braces[extern_C_braces_length++] = brace_nesting;
	      goto new_statement;
	    }
	  c = get_token (fp, &buf);
	}
    }
  for (;;)
    {
      int followingc = getc (fp); /* char following token in buf */
      if (c == IDENTIFIER_TOKEN)
	{
	  int nextc = skip_spaces (fp, followingc);
	  if (nextc == '(')
	    {
	      int nesting = 1;

	      MAKE_SSTRING_SPACE(&rtype, 1);
	      *rtype.ptr = 0;

 	      fprintf (stdout, "%s;%s;%s;",
		       buf.base,
		       in_extern_C_brace || current_extern_C ? "F" : "f",
		       rtype.base);
	      c = skip_spaces (fp, ' ');
	      for (;;)
		{
		  if (c == '(')
		    nesting++;
		  else if (c == ')')
		    if (--nesting == 0)
		      break;
		  if (c == EOF)
		    break;
		  if (c == '\n')
		    c = ' ';
		  putc (c, stdout);	
		  c = getc (fp);
		}
	      fprintf (stdout, ";%s;%d;\n",
		       source_filename.base, source_lineno);
	      goto new_statement;
	    }
	  else if (nextc == ';' && saw_extern)
	    {
 	      fprintf (stdout, "%s;X;%s;\n", buf.base, rtype.base);
	      goto handle_statement;
	    }
	  else
	    ungetc (nextc, fp);
	}
      else if (followingc != EOF)
	ungetc (followingc, fp);
      if (c == ';' || c == '{' || c == '}' || c == EOF)
	goto handle_statement;
      sstring_append (&rtype, &buf);
      if (followingc == ' ' || followingc == '\t' || followingc == '\n')
	SSTRING_PUT(&rtype, ' ');
      c = get_token (fp, &buf);
    }
}
