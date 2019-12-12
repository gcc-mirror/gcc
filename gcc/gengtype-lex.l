/* -*- indented-text -*- */
/* Process source files and output type information.
   Copyright (C) 2002-2019 Free Software Foundation, Inc.

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

%option noinput

%{
#ifdef HOST_GENERATOR_FILE
#include "config.h"
#define GENERATOR_FILE 1
#else
#include "bconfig.h"
#endif
#include "system.h"

#define malloc xmalloc
#define realloc xrealloc

#include "gengtype.h"

#define YY_DECL int yylex (const char **yylval)
#define yyterminate() return EOF_TOKEN

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

CID	[[:alpha:]_][[:alnum:]_]*
WS	[[:space:]]+
HWS	[ \t\r\v\f]*
IWORD	short|long|(un)?signed|char|int|HOST_WIDE_INT|uint64_t|int64_t|bool|size_t|BOOL_BITFIELD|CPPCHAR_SIGNED_T|ino_t|dev_t|HARD_REG_SET
ITYPE	{IWORD}({WS}{IWORD})*
    /* Include '::' in identifiers to capture C++ scope qualifiers.  */
ID	{CID}({HWS}::{HWS}{CID})*
EOID	[^[:alnum:]_]
CXX_KEYWORD inline|public:|private:|protected:|template|operator|friend|static|mutable

%x in_struct in_struct_comment in_comment
%option warn noyywrap nounput nodefault perf-report
%option 8bit never-interactive
%%
  /* Do this on entry to yylex():  */
  *yylval = 0;
  if (lexer_toplevel_done)
    {
      BEGIN(INITIAL);
      lexer_toplevel_done = 0;
    }

  /* Things we look for in skipping mode: */
<INITIAL>{
^{HWS}typedef/{EOID} {
  BEGIN(in_struct);
  return TYPEDEF;
}
^{HWS}struct/{EOID} {
  BEGIN(in_struct);
  return STRUCT;
}
^{HWS}union/{EOID} {
  BEGIN(in_struct);
  return UNION;
}
^{HWS}class/{EOID} {
  BEGIN(in_struct);
  return STRUCT;
}
^{HWS}extern/{EOID} {
  BEGIN(in_struct);
  return EXTERN;
}
^{HWS}static/{EOID} {
  BEGIN(in_struct);
  return STATIC;
}
}

    /* Parsing inside a struct, union or class declaration.  */
<in_struct>{
"/*"				{ BEGIN(in_struct_comment); }
"//".*\n			{ lexer_line.line++; }

{WS}				{ update_lineno (yytext, yyleng); }
\\\n				{ lexer_line.line++; }

"const"/{EOID}			/* don't care */
{CXX_KEYWORD}/{EOID}			|
"~"					|
"^"					|
"&"					{
    *yylval = XDUPVAR (const char, yytext, yyleng, yyleng + 1);
    return IGNORABLE_CXX_KEYWORD;
}
"GTY"/{EOID}			{ return GTY_TOKEN; }
"union"/{EOID}			{ return UNION; }
"struct"/{EOID}			{ return STRUCT; }
"class"/{EOID}			{ return STRUCT; }
"typedef"/{EOID}		{ return TYPEDEF; }
"enum"/{EOID}			{ return ENUM; }
"ptr_alias"/{EOID}	  	{ return PTR_ALIAS; }
"nested_ptr"/{EOID}		{ return NESTED_PTR; }
"user"/{EOID}			{ return USER_GTY; }
[0-9]+				{ return NUM; }

{IWORD}({WS}{IWORD})*/{EOID}		|
"ENUM_BITFIELD"{WS}?"("{WS}?{ID}{WS}?")"	{
  size_t len;

  for (len = yyleng; ISSPACE (yytext[len-1]); len--)
    ;

  *yylval = XDUPVAR (const char, yytext, len, len+1);
  update_lineno (yytext, yyleng);
  return SCALAR;
}

{ID}/{EOID}			{
  *yylval = XDUPVAR (const char, yytext, yyleng, yyleng+1);
  return ID;
}

\"([^"\\]|\\.)*\"		{
  *yylval = XDUPVAR (const char, yytext+1, yyleng-2, yyleng-1);
  return STRING;
}
  /* This "terminal" avoids having to parse integer constant expressions.  */
"["[^\[\]]*"]"			{
  *yylval = XDUPVAR (const char, yytext+1, yyleng-2, yyleng-1);
  return ARRAY;
}
"'"("\\".|[^\\])"'"		{
  *yylval = XDUPVAR (const char, yytext+1, yyleng-2, yyleng);
  return CHAR;
}

"..."				{ return ELLIPSIS; }
[(){},*:<>;=%/|+\!\?\.-]	{ return yytext[0]; }

   /* ignore pp-directives */
^{HWS}"#"{HWS}[a-z_]+[^\n]*\n   {lexer_line.line++;}

.				{
  error_at_line (&lexer_line, "unexpected character `%s'", yytext);
}
}

"/*"			{ BEGIN(in_comment); }
"//".*\n		{ lexer_line.line++; }
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

["/]    		|
<in_struct_comment,in_comment>"*"	{
  error_at_line (&lexer_line, 
		 "unterminated comment or string; unexpected EOF");
}

^{HWS}"#"{HWS}"define"{WS}"GTY(" /* do nothing */

%%

void
yybegin (const char *fname)
{
  yyin = fopen (fname, "r");
  if (yyin == NULL)
    {
      perror (fname);
      exit (1);
    }
  lexer_line.file = input_file_by_name (fname);
  lexer_line.line = 1;
}

void
yyend (void)
{
  fclose (yyin);
}
