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
Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

   Written by Per Bothner <bothner@cygnus.com>, July 1993.  */

#include <stdio.h>
#include <ctype.h>
#include "hconfig.h"
#include "scan.h"

sstring buf;
sstring rtype;
sstring arg_list;

int brace_nesting = 0;

/* The first extern_C_braces_length elements of extern_C_braces
   indicate the (brace nesting levels of) left braces that were
   prefixed by extern "C".  */
int extern_C_braces_length = 0;
char extern_C_braces[20];
#define in_extern_C_brace (extern_C_braces_length>0)

/* True if the function declaration currently being scanned is
   prefixed by extern "C".  */
int current_extern_C = 0;

static void
skip_to_closing_brace (fp)
     FILE *fp;
{
  int nesting = 1;
  for (;;)
    {
      int c = get_token (fp, &buf);
      if (c == EOF)
	break;
      if (c == '{')
	nesting++;
      if (c == '}' && --nesting == 0)
	break;
    }
}

/* This function scans a C source file (actually, the output of cpp),
   reading from FP.  It looks for function declarations, and certain
   other interesting sequences (external variables and macros).  */

int
scan_decls (fp)
     FILE *fp;
{
  int c;
  int saw_extern, saw_inline;

 new_statement:
  c = get_token (fp, &buf);
 handle_statement:
  current_extern_C = 0;
  saw_extern = 0;
  saw_inline = 0;
  if (c == '}')
    {
      /* Pop an 'extern "C"' nesting level, if appropriate.  */
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
      /* For certain interesting macro names, fixproto puts
	 #ifdef FOO
	 __DEFINED_MACRO_FOO
	 #endif
	 into the file to be pre-processed.  So if we see __DEFINED_MACRO_FOO,
	 it means FOO was defined, which we may want to make a note of.  */
      recognized_macro (buf.base+16);
      goto new_statement;
    }
  if (strcmp (buf.base, "inline") == 0)
    {
      saw_inline = 1;
      c = get_token (fp, &buf);
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

      MAKE_SSTRING_SPACE (&rtype, 1);
      *rtype.ptr = 0;

      if (c == IDENTIFIER_TOKEN)
	{
	  int nextc = skip_spaces (fp, followingc);
	  if (nextc == '(')
	    {
	      int nesting = 1;
	      int func_lineno = source_lineno;
	      char *args;

	      arg_list.ptr = arg_list.base;
	      for (;;)
		{
		  c = getc (fp);
		  if (c == '(')
		    nesting++;
		  else if (c == ')')
		    if (--nesting == 0)
		      break;
		  if (c == EOF)
		    break;
		  if (c == '\n')
		    {
		      c = ' ';
		      source_lineno++;
		      lineno++;
		    }
		  SSTRING_PUT (&arg_list, c);
		}
	      SSTRING_PUT (&arg_list, '\0');
	      args = arg_list.base;
	      while (*args == ' ')
		args++;
	      recognized_function (buf.base,
				   (saw_inline ? 'I'
				    : in_extern_C_brace || current_extern_C
				    ? 'F' : 'f'),
				   rtype.base, args,
				   source_filename.base, func_lineno);
	      c = get_token (fp, &buf);
	      if (c == '{')
		{
		  /* skip body of (normally) inline function */
		  skip_to_closing_brace (fp);
		  goto new_statement;
		}
	      goto handle_statement;
	    }
	  else if (nextc == ';' && saw_extern)
	    {
	      recognized_extern (buf.base, rtype.base);
	      goto new_statement;
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
	SSTRING_PUT (&rtype, ' ');
      c = get_token (fp, &buf);
    }
}
