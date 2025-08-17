/* Copyright (C) 2025 Free Software Foundation, Inc.

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
<INSIDE>accepted		return K_ACCEPTED;
<INSIDE>removed			return K_REMOVED;
<INSIDE>stable_removed		return K_STABLE_REMOVED;
<INSIDE>Edition			return K_E_START;
<INSIDE>Edition2018		return K_E_2018;

<INSIDE>[A-Za-z_][A-Za-z0-9_]*	yylval.str = strdup (yytext); return IDENT;
<INSIDE>[1-9][0-9]*		yylval.str = strdup (yytext); return NUM;
<INSIDE>\"[^"]+\"		yylval.str = strdup (yytext); return STR;
<INSIDE>"/""/"			BEGIN (COMMENT);
<INSIDE>[ \n]			/* ignore */
<INSIDE>.			{ fprintf (stderr, "unrecognized character %u\n", (unsigned int) yytext[0]); exit (1); }

<COMMENT>.			/* skip */
<COMMENT>\n			BEGIN (INSIDE);
