/* scan-decls.c - Extracts declarations from cpp output.
   Copyright (C) 1993, 1995, 97-98, 1999 Free Software Foundation, Inc.

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

   Written by Per Bothner <bothner@cygnus.com>, July 1993.  */

#include "hconfig.h"
#include "system.h"
#include "cpplib.h"
#include "scan.h"

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
skip_to_closing_brace (pfile)
     cpp_reader *pfile;
{
  int nesting = 1;
  for (;;)
    {
      enum cpp_token token = cpp_get_token (pfile);
      if (token == CPP_EOF)
	break;
      if (token == CPP_LBRACE)
	nesting++;
      if (token == CPP_RBRACE && --nesting == 0)
	break;
    }
}

/* This function scans a C source file (actually, the output of cpp),
   reading from FP.  It looks for function declarations, and
   external variable declarations.  

   The following grammar (as well as some extra stuff) is recognized:

   declaration:
     (decl-specifier)* declarator ("," declarator)* ";"
   decl-specifier:
     identifier
     keyword
     extern "C"
   declarator:
     (ptr-operator)* dname [ "(" argument-declaration-list ")" ]
   ptr-operator:
     ("*" | "&") ("const" | "volatile")*
   dname:
     identifier

Here dname is the actual name being declared.
*/

int
scan_decls (pfile, argc, argv)
     cpp_reader *pfile;
     int argc ATTRIBUTE_UNUSED;
     char **argv ATTRIBUTE_UNUSED;
{
  int saw_extern, saw_inline;
  int start_written;
  /* If declarator_start is non-zero, it marks the start of the current
     declarator.  If it is zero, we are either still parsing the
     decl-specs, or prev_id_start marks the start of the declarator.  */
  int declarator_start;
  int prev_id_start, prev_id_end;
  enum cpp_token token;

 new_statement:
  CPP_SET_WRITTEN (pfile, 0);
  start_written = 0;
  token = cpp_get_token (pfile);

 handle_statement:
  current_extern_C = 0;
  saw_extern = 0;
  saw_inline = 0;
  if (token == CPP_RBRACE)
    {
      /* Pop an 'extern "C"' nesting level, if appropriate.  */
      if (extern_C_braces_length
	  && extern_C_braces[extern_C_braces_length - 1] == brace_nesting)
	extern_C_braces_length--;
      brace_nesting--;
      goto new_statement;
    }
  if (token == CPP_LBRACE)
    {
      brace_nesting++;
      goto new_statement;
    }
  if (token == CPP_EOF)
    return 0;
  if (token == CPP_SEMICOLON)
    goto new_statement;
  if (token != CPP_NAME)
    goto new_statement;

  prev_id_start = 0;
  declarator_start = 0;
  for (;;)
    {
      switch (token)
	{
	case CPP_LPAREN:
	  /* Looks like this is the start of a formal parameter list.  */
	  if (prev_id_start)
	    {
	      int nesting = 1;
	      int have_arg_list = 0;
	      cpp_buffer *fbuf = cpp_file_buffer (pfile);
	      long func_lineno;
	      cpp_buf_line_and_col (fbuf, &func_lineno, NULL);
	      for (;;)
		{
		  token = cpp_get_token (pfile);
		  if (token == CPP_LPAREN)
		    nesting++;
		  else if (token == CPP_RPAREN)
		    {
		      nesting--;
		      if (nesting == 0)
			break;
		    }
		  else if (token == CPP_EOF)
		    break;
		  else if (token == CPP_NAME || token == CPP_3DOTS)
		    have_arg_list = 1;
		}
	      recognized_function (pfile->token_buffer + prev_id_start,
				   prev_id_end - prev_id_start,
				   (saw_inline ? 'I'
				    : in_extern_C_brace || current_extern_C
				    ? 'F' : 'f'),
				   pfile->token_buffer, prev_id_start,
				   have_arg_list,
				   fbuf->nominal_fname, func_lineno);
	      token = cpp_get_non_space_token (pfile);
	      if (token == CPP_LBRACE)
		{
		  /* skip body of (normally) inline function */
		  skip_to_closing_brace (pfile);
		  goto new_statement;
		}
	      goto maybe_handle_comma;
	    }
	  break;
	case CPP_OTHER:
	  if (CPP_WRITTEN (pfile) == (size_t) start_written + 1
	      && (CPP_PWRITTEN (pfile)[-1] == '*'
		  || CPP_PWRITTEN (pfile)[-1] == '&'))
	    declarator_start = start_written;
	  else
	    goto handle_statement;
	  break;
	case CPP_COMMA:
	case CPP_SEMICOLON:
	  if (prev_id_start && saw_extern)
	    {
	      recognized_extern (pfile->token_buffer + prev_id_start,
				 prev_id_end - prev_id_start,
				 pfile->token_buffer,
				 prev_id_start);
	    }
	  /* ... fall through ...  */
	maybe_handle_comma:
	  if (token != CPP_COMMA)
	    goto new_statement;
#if 0
	handle_comma:
#endif
	  /* Handle multiple declarators in a single declaration,
	     as in:  extern char *strcpy (), *strcat (), ... ; */
	  if (declarator_start == 0)
	    declarator_start = prev_id_start;
	  CPP_SET_WRITTEN (pfile, declarator_start);
	  break;
	case CPP_NAME:
	  /* "inline" and "extern" are recognized but skipped */
	  if (strcmp (pfile->token_buffer, "inline") == 0)
	    {
	      saw_inline = 1;
	      CPP_SET_WRITTEN (pfile, start_written);
	    }
	  if (strcmp (pfile->token_buffer, "extern") == 0)
	    {
	      saw_extern = 1;
	      CPP_SET_WRITTEN (pfile, start_written);
	      token = cpp_get_non_space_token (pfile);
	      if (token == CPP_STRING
		  && strcmp (pfile->token_buffer, "\"C\"") == 0)
		{
		  CPP_SET_WRITTEN (pfile, start_written);
		  current_extern_C = 1;
		  token = cpp_get_non_space_token (pfile);
		  if (token == CPP_LBRACE)
		    {
		      brace_nesting++;
		      extern_C_braces[extern_C_braces_length++]
			= brace_nesting;
		      goto new_statement;
		    }
		}
	      else
		continue;
	      break;
	    }
	  /* This may be the name of a variable or function.  */
	  prev_id_start = start_written;
	  prev_id_end = CPP_WRITTEN (pfile);
	  break;

	case CPP_EOF:
	  return 0;

	case CPP_LBRACE:  case CPP_RBRACE:  case CPP_DIRECTIVE:
	  goto new_statement;  /* handle_statement? */
	  
	case CPP_HSPACE:  case CPP_VSPACE:  case CPP_COMMENT:  case CPP_POP:
	  /* Skip initial white space.  */
	  if (start_written == 0)
	    CPP_SET_WRITTEN (pfile, 0);
	  break;

	 default:
	  prev_id_start = 0;
	}

      start_written = CPP_WRITTEN (pfile);
      token = cpp_get_token (pfile);
    }
}
