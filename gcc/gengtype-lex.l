/* -*- indented-text -*- */
/* Process source files and output type information.
   Copyright (C) 2002, 2003 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

%{
#include "bconfig.h"
#include "coretypes.h"
#include "system.h"

#define malloc xmalloc
#define realloc xrealloc

#include "gengtype.h"
#include "gengtype-yacc.h"

static void update_lineno (const char *l, size_t len);

struct fileloc lexer_line;
int lexer_toplevel_done;

static void 
update_lineno (const char *l, size_t len)
{
  while (len-- > 0)
    if (*l++ == '\n')
      lexer_line.line++;
}

%}

ID	[[:alpha:]_][[:alnum:]_]*
WS	[[:space:]]+
IWORD	short|long|(un)?signed|char|int|HOST_WIDE_INT|bool|size_t|BOOL_BITFIELD
ITYPE	{IWORD}({WS}{IWORD})*

%x in_struct in_struct_comment in_comment in_yacc_escape
%option warn noyywrap nounput nodefault perf-report
%option 8bit never-interactive
%%

[^[:alnum:]_]typedef{WS}(struct|union){WS}{ID}{WS}?[*[:space:]]{WS}?{ID}{WS}?";" {
  char *tagstart;
  size_t taglen;
  char *namestart;
  size_t namelen;
  int is_pointer = 0;
  struct type *t;
  int union_p;

  tagstart = yytext + strlen (" typedef ");
  while (ISSPACE (*tagstart))
    tagstart++;
  union_p = tagstart[0] == 'u';
  tagstart += strlen ("union ");
  while (ISSPACE (*tagstart))
    tagstart++;
  for (taglen = 1; ISIDNUM (tagstart[taglen]); taglen++)
    ;
  for (namestart = tagstart + taglen; 
       ! ISIDNUM (*namestart);
       namestart++)
    if (*namestart == '*')
      is_pointer = 1;
  for (namelen = 1; ISIDNUM (namestart[namelen]); namelen++)
    ;
  t = find_structure (xmemdup (tagstart, taglen, taglen+1), union_p);
  if (is_pointer)
    t = create_pointer (t);
  do_typedef (xmemdup (namestart, namelen, namelen+1), t, &lexer_line);
  update_lineno (yytext, yyleng);
}

[^[:alnum:]_]typedef{WS}{ITYPE}{WS}{ID}{WS}?";" {

  char *namestart;
  size_t namelen;
  struct type *t;
  char *typestart;
  size_t typelen;

  for (namestart = yytext + yyleng - 2; ISSPACE (*namestart); namestart--)
    ;
  for (namelen = 1; !ISSPACE (namestart[-namelen]); namelen++)
    ;
  namestart -= namelen - 1;
  for (typestart = yytext + strlen (" typedef "); 
       ISSPACE(*typestart);
       typestart++)
    ;
  for (typelen = namestart - typestart; 
       ISSPACE(typestart[typelen-1]); 
       typelen--)
    ;

  t = create_scalar_type (typestart, typelen);
  do_typedef (xmemdup (namestart, namelen, namelen+1), t, &lexer_line);
  update_lineno (yytext, yyleng);
}

[^[:alnum:]_]typedef{WS}{ID}{WS}{ID}{WS}PARAMS {
  char *namestart;
  size_t namelen;
  struct type *t;

  for (namestart = yytext + yyleng - 7; ISSPACE (*namestart); namestart--)
    ;
  for (namelen = 1; !ISSPACE (namestart[-namelen]); namelen++)
    ;
  namestart -= namelen - 1;

  t = create_scalar_type ("function type", sizeof ("function type")-1);
  do_typedef (xmemdup (namestart, namelen, namelen+1), t, &lexer_line);
  update_lineno (yytext, yyleng);
}

[^[:alnum:]_]typedef{WS}{ID}{WS}{ID}{WS}"(" {
  char *namestart;
  size_t namelen;
  struct type *t;

  for (namestart = yytext + yyleng - 2; ISSPACE (*namestart); namestart--)
    ;
  for (namelen = 1; !ISSPACE (namestart[-namelen]); namelen++)
    ;
  namestart -= namelen - 1;

  t = create_scalar_type ("function type", sizeof ("function type")-1);
  do_typedef (xmemdup (namestart, namelen, namelen+1), t, &lexer_line);
  update_lineno (yytext, yyleng);
}

[^[:alnum:]_]typedef{WS}{ID}{WS}?"*"?{WS}?"("{WS}?"*"{WS}?{ID}{WS}?")"{WS}?PARAMS {
  char *namestart;
  size_t namelen;
  struct type *t;

  for (namestart = yytext + yyleng - 7; !ISIDNUM (*namestart); namestart--)
    ;
  for (namelen = 1; ISIDNUM (namestart[-namelen]); namelen++)
    ;
  namestart -= namelen - 1;

  t = create_scalar_type ("function type", sizeof ("function type")-1);
  do_typedef (xmemdup (namestart, namelen, namelen+1), t, &lexer_line);
  update_lineno (yytext, yyleng);
}

[^[:alnum:]_]typedef{WS}{ID}{WS}?"*"?{WS}?"("{WS}?"*"{WS}?{ID}{WS}?")"{WS}?"(" {
  char *namestart;
  size_t namelen;
  struct type *t;

  for (namestart = yytext + yyleng - 2; !ISIDNUM (*namestart); namestart--)
    ;
  for (namelen = 1; ISIDNUM (namestart[-namelen]); namelen++)
    ;
  namestart -= namelen - 1;

  t = create_scalar_type ("function type", sizeof ("function type")-1);
  do_typedef (xmemdup (namestart, namelen, namelen+1), t, &lexer_line);
  update_lineno (yytext, yyleng);
}

[^[:alnum:]_](typedef{WS})?(struct|union){WS}{ID}{WS}/"GTY" {
  char *tagstart;
  size_t taglen;
  int typedef_p;
  int union_p;

  typedef_p = yytext[1] == 't';
  if (typedef_p)
    for (tagstart = yytext + strlen (" typedef "); 
	 ISSPACE(*tagstart);
	 tagstart++)
      ;
  else
    tagstart = yytext + 1;

  union_p = tagstart[0] == 'u';
  tagstart += strlen ("union ");
  while (ISSPACE (*tagstart))
    tagstart++;
  for (taglen = 1; ISIDNUM (tagstart[taglen]); taglen++)
    ;

  yylval.t = find_structure (xmemdup (tagstart, taglen, taglen + 1), union_p);
  BEGIN(in_struct);
  update_lineno (yytext, yyleng);
  return typedef_p ? ENT_TYPEDEF_STRUCT : ENT_STRUCT;
}

[^[:alnum:]_](extern|static){WS}/"GTY" {
  BEGIN(in_struct);
  update_lineno (yytext, yyleng);
  return ENT_EXTERNSTATIC;
}

^"%union"{WS}"{"{WS}/"GTY" {
  BEGIN(in_struct);
  update_lineno (yytext, yyleng);
  return ENT_YACCUNION;
}

<in_struct>{

"/*"				{ BEGIN(in_struct_comment); }

^"%{"				{ BEGIN(in_yacc_escape); }

^"@@".*				/* Used for c-parse.in C/ObjC demarcation.  */

{WS}				{ update_lineno (yytext, yyleng); }

"const"/[^[:alnum:]_]		/* don't care */

"GTY"/[^[:alnum:]_]		{ return GTY_TOKEN; }
"union"/[^[:alnum:]_]		{ return UNION; }
"struct"/[^[:alnum:]_]		{ return STRUCT; }
"enum"/[^[:alnum:]_]		{ return ENUM; }
"ptr_alias"/[^[:alnum:]_]	{ return ALIAS; }
[0-9]+				{ return NUM; }
"param"[0-9]*"_is"/[^[:alnum:]_]		{ 
  yylval.s = xmemdup (yytext, yyleng, yyleng+1);
  return PARAM_IS;
}

{IWORD}({WS}{IWORD})*/[^[:alnum:]_]		|
"ENUM_BITFIELD"{WS}?"("{WS}?{ID}{WS}?")"	{
  size_t len;

  for (len = yyleng; ISSPACE (yytext[len-1]); len--)
    ;

  yylval.t = create_scalar_type (yytext, len);
  update_lineno (yytext, yyleng);
  return SCALAR;
}

{ID}/[^[:alnum:]_]		{
  yylval.s = xmemdup (yytext, yyleng, yyleng+1);
  return ID;
}

\"([^"\\]|\\.)*\"		{
  yylval.s = xmemdup (yytext+1, yyleng-2, yyleng-1);
  return STRING;
}
"["[^\[\]]*"]"			{
  yylval.s = xmemdup (yytext+1, yyleng-2, yyleng-1);
  return ARRAY;
}
^"%"{ID}			{
  yylval.s = xmemdup (yytext+1, yyleng-1, yyleng);
  return PERCENT_ID;
}
"'"("\\".|[^\\])"'"		{
  yylval.s = xmemdup (yytext+1, yyleng-2, yyleng);
  return CHAR;
}

[(){},*:<>]			{ return yytext[0]; }

[;=]				{
  if (lexer_toplevel_done)
    {
      BEGIN(INITIAL);
      lexer_toplevel_done = 0;
    }
  return yytext[0];
}

^"%%"				{
  BEGIN(INITIAL);
  return PERCENTPERCENT;
}

.				{
  error_at_line (&lexer_line, "unexpected character `%s'", yytext);
}
}

"/*"			{ BEGIN(in_comment); }
\n			{ lexer_line.line++; }
{ID}			|
"'"("\\".|[^\\])"'"	|
[^"/\n]			/* do nothing */
\"([^"\\]|\\.|\\\n)*\"	{ update_lineno (yytext, yyleng); }
"/"/[^*]		/* do nothing */

<in_comment,in_struct_comment>{
\n		{ lexer_line.line++; }
[^*\n]{16}	|
[^*\n]		/* do nothing */
"*"/[^/]	/* do nothing */
}
<in_comment>"*/"	{ BEGIN(INITIAL); } 
<in_struct_comment>"*/"	{ BEGIN(in_struct); }

<in_yacc_escape>{
\n		{ lexer_line.line++; }
[^%]{16}	|
[^%]		/* do nothing */
"%"/[^}]	/* do nothing */
"%}"		{ BEGIN(in_struct); }
"%"		{
  error_at_line (&lexer_line, 
		 "unterminated %%{; unexpected EOF");
}
}


["/]    		|
<in_struct_comment,in_comment>"*"	{
  error_at_line (&lexer_line, 
		 "unterminated comment or string; unexpected EOF");
}

%%

void
yyerror (const char *s)
{
  error_at_line (&lexer_line, s);
}

void
parse_file (const char *fname)
{
  yyin = fopen (fname, "r");
  lexer_line.file = fname;
  lexer_line.line = 1;
  if (yyin == NULL)
    {
      perror (fname);
      exit (1);
    }
  if (yyparse() != 0)
    exit (1);
  fclose (yyin);
}
