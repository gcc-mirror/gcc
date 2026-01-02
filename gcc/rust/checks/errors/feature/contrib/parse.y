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

#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>

int yylex (void);
void yyerror (char const *);

#include "scan.h"

// expands to three %s parameters
#define UNWRAP_OPT_STR(prefix, s) (s ? prefix "_SOME (" : prefix "_NONE"), (s ? s : ""), (s ? ")" : "")

%}

%union
{
  char *str;
};

%token <str> IDENT STR NUM
%token SCOPE
%token K_SOME K_NONE
%token K_ACTIVE K_ACCEPTED K_REMOVED K_STABLE_REMOVED
%token K_E_START K_E_2018

%type <str> issue
%type <str> edition
%type <str> reason

%%

multi_database: multi_database database
| database
;

database: '(' entry_list ')';

entry_list: entry_list entry ','
| entry ','
;

entry: '(' K_ACTIVE ',' IDENT ',' STR ',' issue ',' edition ')' {
  char *ident_upper = strdup ($4);
  for (size_t i = 0; ident_upper[i]; i++)
    ident_upper[i] = toupper (ident_upper[i]);
  printf ("FEATURE_ACTIVE (\"%s\", %s, %s, %s%s%s, EDITION_%s)\n", $4, ident_upper, $6, UNWRAP_OPT_STR ("ISSUE", $8), $10 ? $10 : "NONE");
  free ($4);
  free (ident_upper);
  free ($6);
  free ($8);
}
| '(' K_ACCEPTED ',' IDENT ',' STR ',' issue ',' K_NONE ')' {
  char *ident_upper = strdup ($4);
  for (size_t i = 0; ident_upper[i]; i++)
    ident_upper[i] = toupper (ident_upper[i]);
  printf ("FEATURE_ACCEPTED (\"%s\", %s, %s, %s%s%s)\n", $4, ident_upper, $6, UNWRAP_OPT_STR ("ISSUE", $8));
  free ($4);
  free (ident_upper);
  free ($6);
  free ($8);
}
| '(' K_REMOVED ',' IDENT ',' STR ',' issue ',' K_NONE ',' reason ')' {
  char *ident_upper;
  // HACK: convert no_debug to F_NO_DEBUG instead
  // since NO_DEBUG is used as an unrelated macro
  if (!strcmp ($4, "no_debug"))
    {
      ident_upper = strdup ("F_NO_DEBUG");
    }
  else
    {
      ident_upper = strdup ($4);
      for (size_t i = 0; ident_upper[i]; i++)
        ident_upper[i] = toupper (ident_upper[i]);
    }
  printf ("FEATURE_REMOVED (\"%s\", %s, %s, %s%s%s, %s%s%s)\n", $4, ident_upper, $6, UNWRAP_OPT_STR ("ISSUE", $8), UNWRAP_OPT_STR ("REASON", $12));
  free ($4);
  free (ident_upper);
  free ($6);
  free ($8);
  free ($12);
}
| '(' K_STABLE_REMOVED ',' IDENT ',' STR ',' issue ',' K_NONE ')' {
  char *ident_upper = strdup ($4);
  for (size_t i = 0; ident_upper[i]; i++)
    ident_upper[i] = toupper (ident_upper[i]);
  printf ("FEATURE_STABLE_REMOVED (\"%s\", %s, %s, %s%s%s)\n", $4, ident_upper, $6, UNWRAP_OPT_STR ("ISSUE", $8));
  free ($4);
  free (ident_upper);
  free ($6);
  free ($8);
}
;

issue: K_SOME '(' NUM ')' { $$ = $3; }
| K_NONE { $$ = NULL; }
;

/* TODO: expand this as needed */
edition: K_NONE { $$ = NULL; }
| K_SOME '(' K_E_START SCOPE K_E_2018 ')' { $$ = "2018"; }
;

reason: K_SOME '(' STR ')' { $$ = $3; }
| K_NONE { $$ = NULL; }
;

%%

void yyerror (const char *msg)
{
  fprintf (stderr, "%s\n", msg);
}

int yywrap (void)
{
  return 1;
}

int main (void)
{
  return yyparse ();
}
