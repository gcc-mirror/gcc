/* Copyright (C) 2025-2026 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

%{

#include "parse.h"

static int p_count = 0;

static void expand_char_lit (char *contents)
{
  // can not consume more space than original
  // only expand \\\n[ws]* -> ""
  // since everything else should be handled by C++
  char *read = contents;
  char *write = contents;
  while (*read)
    {
      if (read[0] == '\\' && read[1] == '\n')
	{
	  read += 2;
	  while (1)
	    {
	      switch (*read)
		{
		case '\t':
		case '\n':
		case '\r':
		case ' ':
		  read++;
		  continue;
		}
	      break;
	    }
	}
      else
        {
	  *(write++) = *(read++);
	}
    }
  *write = '\0';
}

%}

%x INSIDE COMMENT

%%

declare_features!		BEGIN (INSIDE);
.|\n				/* ignore */

<INSIDE>\(			p_count++; return '(';
<INSIDE>\)			if (!--p_count) { BEGIN (0); } return ')';
<INSIDE>,			return ',';
<INSIDE>::			return SCOPE;
<INSIDE>Some			return K_SOME;
<INSIDE>None			return K_NONE;
<INSIDE>active			return K_ACTIVE;
<INSIDE>incomplete		return K_INCOMPLETE;
<INSIDE>accepted		return K_ACCEPTED;
<INSIDE>removed			return K_REMOVED;
<INSIDE>stable_removed		return K_STABLE_REMOVED;
<INSIDE>Edition			return K_E_START;
<INSIDE>Edition2018		return K_E_2018;
<INSIDE>__status_to_bool	return K_STATUS_TO_BOOL;

<INSIDE>$?[A-Za-z_][A-Za-z0-9_]*	yylval.str = strdup (yytext); return IDENT;
<INSIDE>[1-9][0-9]*		yylval.str = strdup (yytext); return NUM;
<INSIDE>\"([^"\\]|\\\"|\\\n)*\"	yylval.str = strdup (yytext); expand_char_lit (yylval.str); return STR;
<INSIDE>"/""/"			BEGIN (COMMENT);
<INSIDE>[ \n]			/* ignore */
<INSIDE>.			{ fprintf (stderr, "unrecognized character %u\n", (unsigned int) yytext[0]); exit (1); }

<COMMENT>.			/* skip */
<COMMENT>\n			BEGIN (INSIDE);
