%{
/* m2.flex implements lexical analysis for Modula-2.

Copyright (C) 2004-2025 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaius.mulley@southwales.ac.uk>.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Modula-2; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "gm2-gcc/gcc-consolidation.h"

#include "GM2Reserved.h"
#include "GM2LexBuf.h"
#include "input.h"
#include "m2options.h"
#include "Gm2linemap.h"

static int cpreprocessor = 0;  /* Replace this with correct getter.  */

#if defined(GM2USEGGC)
#  include "ggc.h"
#endif

#include "timevar.h"

#define START_FILE(F,L)   m2linemap_StartFile(F,L)
#define END_FILE()        m2linemap_EndFile()
#define START_LINE(N,S)   m2linemap_StartLine(N,S)
#define GET_LOCATION(COLUMN_START,COLUMN_END) \
        m2linemap_GetLocationRange(COLUMN_START,COLUMN_END)
#define TIMEVAR_PUSH_LEX  timevar_push (TV_LEX)
#define TIMEVAR_POP_LEX   timevar_pop (TV_LEX)

#ifdef __cplusplus
#define EXTERN extern "C"
#endif

#define FIRST_COLUMN 1

  /* m2.flex provides a lexical analyser for GNU Modula-2.  */

  struct lineInfo {
    char            *linebuf;          /* line contents */
    int              linelen;          /* length */
    int              tokenpos;         /* start position of token within line */
    int              toklen;           /* a copy of yylen (length of token) */
    int              nextpos;          /* position after token */
    int              lineno;           /* line number of this line */
    int              column;           /* first column number of token on this line */
    bool             inuse;            /* do we need to keep this line info? */
    location_t       location;         /* the corresponding gcc location_t */
    struct lineInfo *next;
  };

  struct functionInfo {
    char                *name;         /* function name */
    bool                 module;       /* is it really a module? */
    struct functionInfo *next;         /* list of nested functions */
  };

  static int                  lineno      =1;   /* a running count of the file line number */
  static char                *filename    =NULL;
  static int                  commentLevel=0;
  static int                  commentCLevel=0;
  static struct lineInfo     *currentLine=NULL;
  static struct functionInfo *currentFunction=NULL;
  static bool                 seenFunctionStart=false;
  static bool                 seenEnd=false;
  static bool                 seenModuleStart=false;
  static bool                 isDefinitionModule=false;
  static int                  totalLines=0;

static  void pushLine                 (void);
static  void popLine                  (void);
static  void finishedLine             (void);
static  void resetpos                 (void);
static  void consumeLine              (void);
static  void updatepos                (void);
static  void skippos                  (void);
static  void poperrorskip             (const char *);
static  void endOfComment             (void);
static  void endOfCComment            (void);
static  void splitSlashStar           (void);
static  void handleDate               (void);
static  void handleLine               (void);
static  void handleFile               (void);
static  void handleFunction           (void);
static  void handleColumn             (void);
static  void pushFunction             (char *function, bool module);
static  void popFunction              (void);
static  void checkFunction            (void);
EXTERN  void m2flex_M2Error           (const char *);
EXTERN  location_t m2flex_GetLocation (void);
EXTERN  int  m2flex_GetColumnNo       (void);
EXTERN  bool m2flex_OpenSource        (char *s);
EXTERN  int  m2flex_GetLineNo         (void);
EXTERN  void m2flex_CloseSource       (void);
EXTERN  char *m2flex_GetToken         (void);
EXTERN  void _M2_m2flex_init          (void);
EXTERN  int  m2flex_GetTotalLines     (void);
extern  void  yylex                   (void);

#define YY_DECL void yylex (void)
%}

%option nounput
%x COMMENT COMMENT1 COMMENTC LINE0 LINE1 LINE2

%%

"(*"                       { updatepos();
                             commentLevel=1; pushLine(); skippos();
			     BEGIN COMMENT; }
<COMMENT>"*)"              { endOfComment(); }
<COMMENT>"(*"              { commentLevel++; pushLine(); updatepos(); skippos(); }
<COMMENT>"<*"              { if (commentLevel == 1) {
                               updatepos();
                               pushLine();
                               skippos();
                               BEGIN COMMENT1;
                             } else {
                               updatepos(); skippos();
			     }
                           }
<COMMENT>\n.*              { consumeLine(); }
<COMMENT>.                 { updatepos(); skippos(); }
<COMMENT1>.                { updatepos(); skippos(); }
<COMMENT1>"*>"             { updatepos(); skippos(); finishedLine(); BEGIN COMMENT; }
<COMMENT1>\n.*             { consumeLine(); }
<COMMENT1>"*)"             { poperrorskip("unterminated source code directive, missing *>");
                             endOfComment(); }
<COMMENT1><<EOF>>          { poperrorskip("unterminated source code directive, missing *>"); BEGIN COMMENT; }
<COMMENT><<EOF>>           { poperrorskip("unterminated comment found at the end of the file, missing *)"); BEGIN INITIAL; }

"/*"                       { /* Possibly handle C preprocessor comment.  */
                             if (cpreprocessor)
			       {
				 updatepos ();
				 commentCLevel++;
				 if (commentCLevel == 1)
				   {
				     pushLine ();
				     skippos ();
				   }
				 BEGIN COMMENTC;
			       }
			     else
			       splitSlashStar ();
                           }
<COMMENTC>.                { updatepos(); skippos(); }
<COMMENTC>\n.*             { consumeLine(); }
<COMMENTC>"*/"             { endOfCComment(); }
^\#.*                      { consumeLine(); /* printf("found: %s\n", currentLine->linebuf); */
                             if (M2Options_GetLineDirectives ())
			        BEGIN LINE0;
		           }
\n\#.*                     { consumeLine(); /* printf("found: %s\n", currentLine->linebuf); */
                             if (M2Options_GetLineDirectives ())
			        BEGIN LINE0;
			   }
<LINE0>\#[ \t]*            { updatepos(); }
<LINE0>[0-9]+[ \t]*\"      { updatepos(); lineno=atoi(yytext); BEGIN LINE1; }
<LINE0>\n                  { m2flex_M2Error("missing initial quote after #line directive"); resetpos(); BEGIN INITIAL; }
<LINE0>[^\n]
<LINE1>[^\"\n]+            { m2flex_M2Error("missing final quote after #line directive"); resetpos(); BEGIN INITIAL; }
<LINE1>.*\"                { updatepos();
                             filename = (char *)xrealloc(filename, yyleng+1);
			     strcpy(filename, yytext);
                             filename[yyleng-1] = (char)0;  /* remove trailing quote */
                             START_FILE (filename, lineno);
                             BEGIN LINE2;
                           }
<LINE2>[ \t]*              { updatepos(); }
<LINE2>\n                  { M2LexBuf_SetFile(filename); updatepos(); BEGIN INITIAL; }
<LINE2>2[ \t]*\n           { M2LexBuf_SetFile(filename); updatepos(); BEGIN INITIAL; }
<LINE2>1[ \t]*\n           { M2LexBuf_SetFile(filename); updatepos(); BEGIN INITIAL; }
<LINE2>1[ \t]*.*\n         { M2LexBuf_SetFile(filename); updatepos(); BEGIN INITIAL; }
<LINE2>2[ \t]*.*\n         { M2LexBuf_SetFile(filename); updatepos(); BEGIN INITIAL; }
<LINE2>3[ \t]*.*\n         { M2LexBuf_SetFile(filename); updatepos(); BEGIN INITIAL; }

\n[^\#].*                  { consumeLine(); /* printf("found: %s\n", currentLine->linebuf); */ }
\n                         { consumeLine(); /* printf("found: %s\n", currentLine->linebuf); */ }

\"[^\"\n]*\"               { updatepos(); M2LexBuf_AddTokCharStar(M2Reserved_stringtok, yytext); return; }
\"[^\"\n]*$                { updatepos();
                             m2flex_M2Error("missing terminating quote, \"");
			     M2LexBuf_AddTokCharStar(M2Reserved_stringtok, yytext);
                             resetpos(); return;
                           }

'[^'\n]*'                  { updatepos(); M2LexBuf_AddTokCharStar(M2Reserved_stringtok, yytext); return; }
'[^'\n]*$                  { updatepos();
                             m2flex_M2Error("missing terminating quote, '");
			     M2LexBuf_AddTokCharStar(M2Reserved_stringtok, yytext);
                             resetpos(); return;
                           }

<<EOF>>                    { updatepos(); M2LexBuf_AddTok(M2Reserved_eoftok); return; }
\+                         { updatepos(); M2LexBuf_AddTok(M2Reserved_plustok); return; }
-                          { updatepos(); M2LexBuf_AddTok(M2Reserved_minustok); return; }
"*"                        { updatepos(); M2LexBuf_AddTok(M2Reserved_timestok); return; }
\/                         { updatepos(); M2LexBuf_AddTok(M2Reserved_dividetok); return; }
:=                         { updatepos(); M2LexBuf_AddTok(M2Reserved_becomestok); return; }
\&                         { updatepos(); M2LexBuf_AddTok(M2Reserved_ambersandtok); return; }
\.                         { updatepos(); M2LexBuf_AddTok(M2Reserved_periodtok); return; }
\,                         { updatepos(); M2LexBuf_AddTok(M2Reserved_commatok); return; }
\;                         { updatepos(); M2LexBuf_AddTok(M2Reserved_semicolontok); return; }
\(                         { updatepos(); M2LexBuf_AddTok(M2Reserved_lparatok); return; }
\)                         { updatepos(); M2LexBuf_AddTok(M2Reserved_rparatok); return; }
\[                         { updatepos(); M2LexBuf_AddTok(M2Reserved_lsbratok); return; }
\]                         { updatepos(); M2LexBuf_AddTok(M2Reserved_rsbratok); return; }
\(\!                       { updatepos(); M2LexBuf_AddTok(M2Reserved_lsbratok); return; }
\!\)                       { updatepos(); M2LexBuf_AddTok(M2Reserved_rsbratok); return; }
\^                         { updatepos(); M2LexBuf_AddTok(M2Reserved_uparrowtok); return; }
\@                         { updatepos(); M2LexBuf_AddTok(M2Reserved_uparrowtok); return; }
\{                         { updatepos(); M2LexBuf_AddTok(M2Reserved_lcbratok); return; }
\}                         { updatepos(); M2LexBuf_AddTok(M2Reserved_rcbratok); return; }
\(\:                       { updatepos(); M2LexBuf_AddTok(M2Reserved_lcbratok); return; }
\:\)                       { updatepos(); M2LexBuf_AddTok(M2Reserved_rcbratok); return; }
\'                         { updatepos(); M2LexBuf_AddTok(M2Reserved_singlequotetok); return; }
\=                         { updatepos(); M2LexBuf_AddTok(M2Reserved_equaltok); return; }
\#                         { updatepos(); M2LexBuf_AddTok(M2Reserved_hashtok); return; }
\<                         { updatepos(); M2LexBuf_AddTok(M2Reserved_lesstok); return; }
\>                         { updatepos(); M2LexBuf_AddTok(M2Reserved_greatertok); return; }
\<\>                       { updatepos(); M2LexBuf_AddTok(M2Reserved_lessgreatertok); return; }
\<\=                       { updatepos(); M2LexBuf_AddTok(M2Reserved_lessequaltok); return; }
\>\=                       { updatepos(); M2LexBuf_AddTok(M2Reserved_greaterequaltok); return; }
"<*"                       { updatepos(); M2LexBuf_AddTok(M2Reserved_ldirectivetok); return; }
"*>"                       { updatepos(); M2LexBuf_AddTok(M2Reserved_rdirectivetok); return; }
\.\.                       { updatepos(); M2LexBuf_AddTok(M2Reserved_periodperiodtok); return; }
\.\.\.                     { updatepos(); M2LexBuf_AddTok(M2Reserved_periodperiodperiodtok); return; }
\:                         { updatepos(); M2LexBuf_AddTok(M2Reserved_colontok); return; }
\"                         { updatepos(); M2LexBuf_AddTok(M2Reserved_doublequotestok); return; }
\|                         { updatepos(); M2LexBuf_AddTok(M2Reserved_bartok); return; }
\!                         { updatepos(); M2LexBuf_AddTok(M2Reserved_bartok); return; }
\~                         { updatepos(); M2LexBuf_AddTok(M2Reserved_nottok); return; }
AND                        { updatepos(); M2LexBuf_AddTok(M2Reserved_andtok); return; }
ARRAY                      { updatepos(); M2LexBuf_AddTok(M2Reserved_arraytok); return; }
BEGIN                      { updatepos(); M2LexBuf_AddTok(M2Reserved_begintok); return; }
BY                         { updatepos(); M2LexBuf_AddTok(M2Reserved_bytok); return; }
CASE                       { updatepos(); M2LexBuf_AddTok(M2Reserved_casetok); return; }
CONST                      { updatepos(); M2LexBuf_AddTok(M2Reserved_consttok); return; }
DEFINITION                 { updatepos(); isDefinitionModule = true;
                             M2LexBuf_AddTok(M2Reserved_definitiontok); return; }
DIV                        { updatepos(); M2LexBuf_AddTok(M2Reserved_divtok); return; }
DO                         { updatepos(); M2LexBuf_AddTok(M2Reserved_dotok); return; }
ELSE                       { updatepos(); M2LexBuf_AddTok(M2Reserved_elsetok); return; }
ELSIF                      { updatepos(); M2LexBuf_AddTok(M2Reserved_elsiftok); return; }
END                        { updatepos(); seenEnd=true;
                             M2LexBuf_AddTok(M2Reserved_endtok); return; }
EXCEPT                     { updatepos(); M2LexBuf_AddTok(M2Reserved_excepttok); return; }
EXIT                       { updatepos(); M2LexBuf_AddTok(M2Reserved_exittok); return; }
EXPORT                     { updatepos(); M2LexBuf_AddTok(M2Reserved_exporttok); return; }
FINALLY                    { updatepos(); M2LexBuf_AddTok(M2Reserved_finallytok); return; }
FOR                        { updatepos(); M2LexBuf_AddTok(M2Reserved_fortok); return; }
FORWARD                    { updatepos(); M2LexBuf_AddTok(M2Reserved_forwardtok); return; }
FROM                       { updatepos(); M2LexBuf_AddTok(M2Reserved_fromtok); return; }
IF                         { updatepos(); M2LexBuf_AddTok(M2Reserved_iftok); return; }
IMPLEMENTATION             { updatepos(); M2LexBuf_AddTok(M2Reserved_implementationtok); return; }
IMPORT                     { updatepos(); M2LexBuf_AddTok(M2Reserved_importtok); return; }
IN                         { updatepos(); M2LexBuf_AddTok(M2Reserved_intok); return; }
LOOP                       { updatepos(); M2LexBuf_AddTok(M2Reserved_looptok); return; }
MOD                        { updatepos(); M2LexBuf_AddTok(M2Reserved_modtok); return; }
MODULE                     { updatepos(); seenModuleStart=true;
                             M2LexBuf_AddTok(M2Reserved_moduletok); return; }
NOT                        { updatepos(); M2LexBuf_AddTok(M2Reserved_nottok); return; }
OF                         { updatepos(); M2LexBuf_AddTok(M2Reserved_oftok); return; }
OR                         { updatepos(); M2LexBuf_AddTok(M2Reserved_ortok); return; }
PACKEDSET                  { updatepos(); M2LexBuf_AddTok(M2Reserved_packedsettok); return; }
POINTER                    { updatepos(); M2LexBuf_AddTok(M2Reserved_pointertok); return; }
PROCEDURE                  { updatepos(); seenFunctionStart=true;
                             M2LexBuf_AddTok(M2Reserved_proceduretok); return; }
QUALIFIED                  { updatepos(); M2LexBuf_AddTok(M2Reserved_qualifiedtok); return; }
UNQUALIFIED                { updatepos(); M2LexBuf_AddTok(M2Reserved_unqualifiedtok); return; }
RECORD                     { updatepos(); M2LexBuf_AddTok(M2Reserved_recordtok); return; }
REM                        { updatepos(); M2LexBuf_AddTok(M2Reserved_remtok); return; }
REPEAT                     { updatepos(); M2LexBuf_AddTok(M2Reserved_repeattok); return; }
RETRY                      { updatepos(); M2LexBuf_AddTok(M2Reserved_retrytok); return; }
RETURN                     { updatepos(); M2LexBuf_AddTok(M2Reserved_returntok); return; }
SET                        { updatepos(); M2LexBuf_AddTok(M2Reserved_settok); return; }
THEN                       { updatepos(); M2LexBuf_AddTok(M2Reserved_thentok); return; }
TO                         { updatepos(); M2LexBuf_AddTok(M2Reserved_totok); return; }
TYPE                       { updatepos(); M2LexBuf_AddTok(M2Reserved_typetok); return; }
UNTIL                      { updatepos(); M2LexBuf_AddTok(M2Reserved_untiltok); return; }
VAR                        { updatepos(); M2LexBuf_AddTok(M2Reserved_vartok); return; }
WHILE                      { updatepos(); M2LexBuf_AddTok(M2Reserved_whiletok); return; }
WITH                       { updatepos(); M2LexBuf_AddTok(M2Reserved_withtok); return; }
ASM                        { updatepos(); M2LexBuf_AddTok(M2Reserved_asmtok); return; }
VOLATILE                   { updatepos(); M2LexBuf_AddTok(M2Reserved_volatiletok); return; }
\_\_DATE\_\_               { updatepos(); handleDate(); return; }
\_\_LINE\_\_               { updatepos(); handleLine(); return; }
\_\_FILE\_\_               { updatepos(); handleFile(); return; }
\_\_FUNCTION\_\_           { updatepos(); handleFunction(); return; }
\_\_COLUMN\_\_             { updatepos(); handleColumn(); return; }
\_\_ATTRIBUTE\_\_          { updatepos(); M2LexBuf_AddTok(M2Reserved_attributetok); return; }
\_\_BUILTIN\_\_            { updatepos(); M2LexBuf_AddTok(M2Reserved_builtintok); return; }
\_\_INLINE\_\_             { updatepos(); M2LexBuf_AddTok(M2Reserved_inlinetok); return; }


(([0-9]*\.[0-9]+)(E[+-]?[0-9]+)?) { updatepos(); M2LexBuf_AddTokCharStar(M2Reserved_realtok, yytext); return; }
[0-9]*\.E[+-]?[0-9]+       { updatepos(); M2LexBuf_AddTokCharStar(M2Reserved_realtok, yytext); return; }
[a-zA-Z_][a-zA-Z0-9_]*     { checkFunction(); updatepos(); M2LexBuf_AddTokCharStar(M2Reserved_identtok, yytext); return; }
[0-9]+                     { updatepos(); M2LexBuf_AddTokCharStar(M2Reserved_integertok, yytext); return; }
[0-1]+A                    { updatepos(); M2LexBuf_AddTokCharStar(M2Reserved_integertok, yytext); return; }
[0-9]+B                    { updatepos(); M2LexBuf_AddTokCharStar(M2Reserved_integertok, yytext); return; }
[0-9]+C                    { updatepos(); M2LexBuf_AddTokCharStar(M2Reserved_integertok, yytext); return; }
[0-9A-F]+H                 { updatepos(); M2LexBuf_AddTokCharStar(M2Reserved_integertok, yytext); return; }
[\t\r ]+                   { currentLine->tokenpos += yyleng;  /* Ignore space.  */; }
.                          { updatepos(); m2flex_M2Error("unrecognised symbol"); skippos(); }

%%

/* have removed the -? from the beginning of the real/integer constant literal rules */

/*
 *  hand built routines
 */

/*
 *  handleFile - handles the __FILE__ construct by wraping it in double quotes and putting
 *               it into the token buffer as a string.
 */

static void handleFile (void)
{
  char *s = (char *)alloca(strlen(filename)+2+1);

  strcpy(s, "\"");
  strcat(s, filename);
  strcat(s, "\"");
  M2LexBuf_AddTokCharStar(M2Reserved_stringtok, s);
}

/*
 *  handleLine - handles the __LINE__ construct by passing an integer to
 *               the token buffer.
 */

static void handleLine (void)
{
  M2LexBuf_AddTokInteger(M2Reserved_integertok, lineno);
}

/*
 *  handleColumn - handles the __COLUMN__ construct by passing an integer to
 *                 the token buffer.
 */

static void handleColumn (void)
{
  M2LexBuf_AddTokInteger(M2Reserved_integertok, m2flex_GetColumnNo());
}

/*
 *  handleDate - handles the __DATE__ construct by passing the date
 *               as a string to the token buffer.
 */

static void handleDate (void)
{
  time_t  clock = time ((time_t *)0);
  char   *sdate = ctime (&clock);
  char   *s     = (char *) alloca (strlen (sdate) + 2 + 1);
  char   *p     = index (sdate, '\n');

  if (p != NULL) {
    *p = (char) 0;
  }
  strcpy(s, "\"");
  strcat(s, sdate);
  strcat(s, "\"");
  M2LexBuf_AddTokCharStar (M2Reserved_stringtok, s);
}

/*
 *  handleFunction - handles the __FUNCTION__ construct by wrapping
 *                   it in double quotes and putting it into the token
 *                   buffer as a string.
 */

static void handleFunction (void)
{
  if (currentFunction == NULL)
    M2LexBuf_AddTokCharStar(M2Reserved_stringtok, const_cast<char *>("\"\""));
  else if (currentFunction->module) {
    char *s = (char *) alloca(strlen(yytext) +
			      strlen("\"module  initialization\"") + 1);
    strcpy(s, "\"module ");
    strcat(s, currentFunction->name);
    strcat(s, " initialization\"");
    M2LexBuf_AddTokCharStar(M2Reserved_stringtok, s);
  } else {
    char *function = currentFunction->name;
    char *s = (char *)alloca(strlen(function)+2+1);
    strcpy(s, "\"");
    strcat(s, function);
    strcat(s, "\"");
    M2LexBuf_AddTokCharStar(M2Reserved_stringtok, s);
  }
}

/*
 *  pushFunction - pushes the function name onto the stack.
 */

static void pushFunction (char *function, bool module)
{
  if (currentFunction == NULL) {
    currentFunction = (struct functionInfo *)xmalloc (sizeof (struct functionInfo));
    currentFunction->name = xstrdup(function);
    currentFunction->next = NULL;
    currentFunction->module = module;
  } else {
    struct functionInfo *f = (struct functionInfo *)xmalloc (sizeof (struct functionInfo));
    f->name = xstrdup(function);
    f->next = currentFunction;
    f->module = module;
    currentFunction = f;
  }
}

/*
 *  popFunction - pops the current function.
 */

static void popFunction (void)
{
  if (currentFunction != NULL && currentFunction->next != NULL) {
    struct functionInfo *f = currentFunction;

    currentFunction = currentFunction->next;
    if (f->name != NULL)
      free(f->name);
    free(f);
  }
}

/*
 *  endOfComment - handles the end of comment
 */

static void endOfComment (void)
{
  commentLevel--;
  updatepos();
  skippos();
  if (commentLevel==0) {
    BEGIN INITIAL;
    finishedLine();
  } else
    popLine();
}

/*
 *  endOfCComment - handles the end of C comment.
 */

static void endOfCComment (void)
{
  commentCLevel = 0;
  updatepos();
  skippos();
  BEGIN INITIAL;
  finishedLine();
}

/*
 *  m2flex_M2Error - displays the error message, s, after the code line and pointer
 *                   to the erroneous token.
 */

EXTERN void m2flex_M2Error (const char *s)
{
  if (currentLine->linebuf != NULL) {
    int i=1;

    printf("%s:%d:%s\n", filename, currentLine->lineno, currentLine->linebuf);
    printf("%s:%d:%*s", filename, currentLine->lineno, 1+currentLine->tokenpos, "^");
    while (i<currentLine->toklen) {
      putchar('^');
      i++;
    }
    putchar('\n');
  }
  if (s == NULL)
    printf("%s:%d\n", filename, currentLine->lineno);
  else
    printf("%s:%d:%s\n", filename, currentLine->lineno, s);
}

static void poperrorskip (const char *s)
{
  int nextpos =currentLine->nextpos;
  int tokenpos=currentLine->tokenpos;

  popLine();
  m2flex_M2Error(s);
  if (currentLine != NULL) {
    currentLine->nextpos  = nextpos;
    currentLine->tokenpos = tokenpos;
  }
}

/* skipnewline skips all '\n' at the start of the line and returns
   the new position.  */

static
char *
skipnewline (char *line)
{
  while (((*line) != (char)0) && ((*line) == '\n'))
    line++;
  return line;
}

/* traceLine display the source line providing -fdebug-trace-line was
   enabled.  */

static
void
traceLine (void)
{
  if (M2Options_GetDebugTraceLine ())
    {
      char *line = skipnewline (currentLine->linebuf);
      if (filename == NULL)
	printf("<stdin>:%d:%s\n", currentLine->lineno, line);
      else
	printf("%s:%d:%s\n", filename, currentLine->lineno, line);
    }
}

/*
 *  consumeLine - reads a line into a buffer, it then pushes back the whole
 *                line except the initial \n.
 */

static void consumeLine (void)
{
  if (currentLine->linelen<yyleng) {
    currentLine->linebuf = (char *)xrealloc (currentLine->linebuf, yyleng);
    currentLine->linelen = yyleng;
  }
  strcpy(currentLine->linebuf, yytext+1);  /* copy all except the initial \n */
  lineno++;
  totalLines++;
  currentLine->lineno = lineno;
  currentLine->tokenpos=0;
  currentLine->nextpos=0;
  currentLine->column=FIRST_COLUMN;
  START_LINE (lineno, yyleng);
  yyless(1);                  /* push back all but the \n */
  traceLine ();
}

static void assert_location (location_t location ATTRIBUTE_UNUSED)
{
#if 0
  if ((location != BUILTINS_LOCATION) && (location != UNKNOWN_LOCATION) && (! M2Options_GetCpp ())) {
     expanded_location xl = expand_location (location);
     if (xl.line != currentLine->lineno) {
       m2flex_M2Error ("mismatched gcc location and front end token number");
     }
  }
#endif
}

/*
 *  splitSlashStar - called if we are not tokenizing source code after it
 *                   has been preprocessed by cpp.  It is only called
 *                   if the current token was a / immediately followed by * and
 *                   therefore it will be split into two m2 tokens:  / and *.
 */

static void splitSlashStar (void)
{
  seenFunctionStart    = false;
  seenEnd              = false;
  seenModuleStart      = false;
  currentLine->nextpos = currentLine->tokenpos+1;  /* "/".  */
  currentLine->toklen  = 1;
  currentLine->column = currentLine->tokenpos+1;
  currentLine->location =
    M2Options_OverrideLocation (GET_LOCATION (currentLine->column,
                                              currentLine->column+currentLine->toklen-1));
  assert_location (GET_LOCATION (currentLine->column,
                                 currentLine->column+currentLine->toklen-1));
  M2LexBuf_AddTok (M2Reserved_dividetok);
  currentLine->nextpos = currentLine->tokenpos+1;  /* "*".  */
  currentLine->toklen  = 1;
  currentLine->column = currentLine->tokenpos+1;
  currentLine->location =
    M2Options_OverrideLocation (GET_LOCATION (currentLine->column,
                                              currentLine->column+currentLine->toklen-1));
  assert_location (GET_LOCATION (currentLine->column,
                                 currentLine->column+currentLine->toklen-1));
  M2LexBuf_AddTok (M2Reserved_timestok);
}


/*
 *  updatepos - updates the current token position.
 *              Should be used when a rule matches a token.
 */

static void updatepos (void)
{
  seenFunctionStart    = false;
  seenEnd              = false;
  seenModuleStart      = false;
  currentLine->nextpos = currentLine->tokenpos+yyleng;
  currentLine->toklen  = yyleng;
  currentLine->column = currentLine->tokenpos+1;
  currentLine->location =
    M2Options_OverrideLocation (GET_LOCATION (currentLine->column,
                                              currentLine->column+currentLine->toklen-1));
  assert_location (GET_LOCATION (currentLine->column,
                                 currentLine->column+currentLine->toklen-1));
}

/*
 *  checkFunction - checks to see whether we have seen the start
 *                  or end of a function.
 */

static void checkFunction (void)
{
  if (! isDefinitionModule) {
    if (seenModuleStart)
      pushFunction(yytext, 1);
    if (seenFunctionStart)
      pushFunction(yytext, 0);
    if (seenEnd && currentFunction != NULL &&
	(strcmp(currentFunction->name, yytext) == 0))
      popFunction();
  }
  seenFunctionStart = false;
  seenEnd           = false;
  seenModuleStart   = false;
}

/*
 *  skippos - skips over this token. This function should be called
 *            if we are not returning and thus not calling getToken.
 */

static void skippos (void)
{
  currentLine->tokenpos = currentLine->nextpos;
}

/*
 *  initLine - initializes a currentLine
 */

static void initLine (void)
{
  currentLine = (struct lineInfo *)xmalloc (sizeof(struct lineInfo));

  if (currentLine == NULL)
    perror("xmalloc");
  currentLine->linebuf    = NULL;
  currentLine->linelen    = 0;
  currentLine->tokenpos   = 0;
  currentLine->toklen     = 0;
  currentLine->nextpos    = 0;
  currentLine->lineno = lineno;
  currentLine->column     = FIRST_COLUMN;
  currentLine->inuse      = true;
  currentLine->next       = NULL;
}

/*
 *  pushLine - pushes a new line structure.
 */

static void pushLine (void)
{
  if (currentLine == NULL)
    initLine();
  else if (currentLine->inuse) {
      struct lineInfo *l = (struct lineInfo *)xmalloc (sizeof(struct lineInfo));

      if (currentLine->linebuf == NULL) {
	l->linebuf  = NULL;
	l->linelen  = 0;
      } else {
	l->linebuf    = (char *)xstrdup (currentLine->linebuf);
	l->linelen    = strlen (l->linebuf)+1;
      }
      l->tokenpos   = currentLine->tokenpos;
      l->toklen     = currentLine->toklen;
      l->nextpos    = currentLine->nextpos;
      l->lineno = currentLine->lineno;
      l->column     = currentLine->column;
      l->next       = currentLine;
      currentLine   = l;
  }
  currentLine->inuse = true;
}

/*
 *  popLine - pops a line structure.
 */

static void popLine (void)
{
  if (currentLine != NULL) {
    struct lineInfo *l = currentLine;

    if (currentLine->linebuf != NULL)
      free(currentLine->linebuf);
    currentLine = l->next;
    free(l);
  }
}

/*
 *  resetpos - resets the position of the next token to the start of the line.
 */

static void resetpos (void)
{
  if (currentLine != NULL)
    currentLine->nextpos = 0;
}

/*
 *  finishedLine - indicates that the current line does not need to be
 *                 preserved when a pushLine occurs.
 */

static void finishedLine (void)
{
  currentLine->inuse = false;
}

/*
 *  m2flex_GetToken - returns a new token.
 */

EXTERN char *m2flex_GetToken (void)
{
  TIMEVAR_PUSH_LEX;
  if (currentLine == NULL)
    initLine();
  currentLine->tokenpos = currentLine->nextpos;
  yylex();
  TIMEVAR_POP_LEX;
  return yytext;
}

/*
 *  CloseSource - provided for semantic sugar
 */

EXTERN void m2flex_CloseSource (void)
{
  END_FILE ();
}

/*
 *  OpenSource - returns true if file s can be opened and
 *               all tokens are taken from this file.
 */

EXTERN bool m2flex_OpenSource (char *s)
{
  FILE *f = fopen(s, "r");

  if (f == NULL)
    return( false );
  else {
    isDefinitionModule = false;
    while (currentFunction != NULL)
      {
	struct functionInfo *f = currentFunction;
        currentFunction = f->next;
        if (f->name != NULL)
  	  free(f->name);
      	free(f);
      }
    yy_delete_buffer (YY_CURRENT_BUFFER);
    yy_switch_to_buffer (yy_create_buffer(f, YY_BUF_SIZE));
    filename = xstrdup (s);
    lineno = 1;
    if (currentLine == NULL)
      pushLine ();
    else
      currentLine->lineno = lineno;
    START_FILE (filename, lineno);
    BEGIN INITIAL; resetpos ();
    return true;
  }
}

/*
 *  m2flex_GetLineNo - returns the current line number.
 */

EXTERN int m2flex_GetLineNo (void)
{
  if (currentLine == NULL)
    return 0;
  else
    return currentLine->lineno;
}

/*
 *  m2flex_GetColumnNo - returns the column where the current
 *                       token starts.
 */

EXTERN int m2flex_GetColumnNo (void)
{
  if (currentLine == NULL)
    return FIRST_COLUMN;
  else
    return currentLine->column;
}

/*
 *  m2flex_GetLocation - returns the gcc location_t of the current token.
 */

EXTERN location_t m2flex_GetLocation (void)
{
  if (currentLine == NULL)
    return 0;
  else
    return currentLine->location;
}

/*
 *  GetTotalLines - returns the total number of lines parsed.
 */

EXTERN int m2flex_GetTotalLines (void)
{
  return totalLines;
}

/*
 *  yywrap is called when end of file is seen. We push an eof token
 *         and tell the lexical analysis to stop.
 */

int yywrap (void)
{
  updatepos(); M2LexBuf_AddTok(M2Reserved_eoftok); return 1;
}

EXTERN void _M2_m2flex_init (void) {}
EXTERN void _M2_m2flex_fini (void) {}
