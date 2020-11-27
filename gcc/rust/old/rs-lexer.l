%{
  /* This file is part of GCC.

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

#include "rust.h"
#include "y.rs.h"

extern int yydebug;
extern int yyparse (void);
%}

DIGIT        [0-9]
ID           [_a-zA-Z][a-zA_Z0-9_$]*
QSTRING      \"[^\"\n]*[\"\n]

%x comment
%option yylineno noyywrap nounput 8bit never-interactive
%%

"/*"                    {  BEGIN (comment); }
<comment>[^*\n]*        {  /* eat it up flex you be le hungry maybe? */ }
<comment>"*"+[^*/\n]*   {  /* eat up boyo */ }
<comment>\n             {  ++yylineno; }
<comment>"*"+"/"        {  BEGIN (INITIAL); }

as                      {  return AS; }
pub                     {  return PUB; }
break                   {  return BREAK; }
return                  {  return RETURN; }
continue                {  return CONTINUE; }
do                      {  return DO; }
fn                      {  return DEFUN; }
let                     {  return LET; }
mut                     {  return MUT; }
while                   {  return WHILE; }
loop                    {  return LOOP; }
static                  {  return STATIC; }
struct                  {  return STRUCT; }
if                      {  return IF; }
"else if"               {  return ELIF; }
else                    {  return ELSE; }
impl                    {  return IMPL; }
self                    {  return SELF; }
bool                    {  return TYPE_BOOL; }
int                     {  return TYPE_INT; }
float                   {  return TYPE_FLOAT; }
uint                    {  return TYPE_UINT; }
enum                    {  return ENUM; }
match                   {  return MATCH; }
true                    {  return XTRUE; }
false                   {  return XFALSE; }
trait                   {  return TRAIT; }
for                     {  return FOR; }
\[                      {  return '['; }
\]                      {  return ']'; }
\(                      {  return '('; }
\)                      {  return ')'; }
\{                      {  return '{'; }
\}                      {  return '}'; }
"->"                    {  return RTYPE; }
"=>"                    {  return GOES; }
";"                     { return ';'; }
","                     { return ','; }
"."                     { return '.'; }
":"                     { return ':'; }
"::"                    { return ACC; }
"="                     { return '='; }
"+"                     { return '+'; }
"-"                     { return '-'; }
"/"                     { return '/'; }
"*"                     { return '*'; }
"|"                     { return '|'; }
"~"                     { return '~'; }
"&"                     { return '&'; }
"=="                    { return EQUAL_EQUAL; }
"!="                    { return NOT_EQUAL; }
"<"                     { return '<'; }
"<="                    { return LESS_EQUAL; }
">"                     { return '>'; }
">="                    { return GREATER_EQUAL; }

\/\/.*                  ;  /* // style comment */

{QSTRING}               {
  yylval.string = xstrdup (yytext + 1);
  if (yylval.string [yyleng - 2] != '\"')
    error ("Un-termintated character string!\n");
  else
    yylval.string [yyleng - 2] = '\0';
  return STRING;
}

{DIGIT}+                {
  mpfr_t x;
  mpfr_init2 (x, 32);
  if (mpfr_set_str (x, yytext, 10, GMP_RNDU))
    {
      fatal_error ("error initilizing integer value <%s>!\n", yytext);
    }
  yylval.integer = mpfr_get_si (x, GMP_RNDU);
  mpfr_clear (x);
  return INTEGER;
}

{DIGIT}+"."{DIGIT}+    {
  mpfr_t x;
  mpfr_init2 (x, 32);
  if (mpfr_set_str (x, yytext, 10, GMP_RNDU))
    {
      fatal_error ("error initilizing float value <%s>!\n", yytext);
    }
  yylval.ffloat = mpfr_get_flt (x, GMP_RNDU);
  mpfr_clear (x);
  return FLOAT;
}

{ID}                    {
  yylval.string = xstrdup (yytext);
  return IDENTIFIER;
}

.           {  }
\n          {  }

%%

bool grs_do_compile (const char * in)
{
    bool retval = true;
    FILE * fd = fopen (in, "rb");
    if (fd)
    {
	yyin = fd;
        // yydebug = 1;
        linemap_add (line_table, LC_ENTER, 0, in, 0);
	retval = yyparse ();
	fclose (fd);
        linemap_add (line_table, LC_LEAVE, 0, NULL, 0);
	yylex_destroy ();
    }
    else
    {
	fprintf (stderr, "error opening file %s\n", in);
	retval = false;
    }
    return retval;
}

