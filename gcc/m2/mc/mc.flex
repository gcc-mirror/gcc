%{
/* mc.flex implements lexical analysis for Modula-2.

Copyright (C) 2004-2024 Free Software Foundation, Inc.
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


#include "mcReserved.h"
#include "mcLexBuf.h"
#include "mcComment.h"

#include <time.h>
#include <ctype.h>

#ifndef alloca
#ifdef __GNUC__
#define alloca __builtin_alloca
#endif
#endif

#if !defined(TRUE)
#  define TRUE (1==1)
#endif
#if !defined(FALSE)
#  define FALSE (1==0)
#endif

#define START_FILE(F,L)
#define END_FILE()
#define START_LINE(N,S)
#define TIMEVAR_PUSH_LEX
#define TIMEVAR_POP_LEX


  /* m2.flex - provides a lexical analyser for GNU Modula-2.  */

  struct lineInfo {
    char            *linebuf;          /* Line contents.  */
    unsigned int     linelen;          /* Length.  */
    int              tokenpos;         /* Start position of token within line.  */
    int              toklen;           /* A copy of yylen (length of token).  */
    int              nextpos;          /* Position after token.  */
    int              actualline;       /* Line number of this line.  */
    int              column;           /* First column number of token on this line.  */
    int              inuse;            /* Do we need to keep this line info?  */
    struct lineInfo *next;
  };

  struct functionInfo {
    char                *name;         /* Function name.  */
    int                  module;       /* Is it really a module?  */
    struct functionInfo *next;         /* List of nested functions.  */
  };

  static int                  lineno      =1;   /* A running count of the file line number.  */
  static char                *filename    =NULL;
  static int                  commentLevel=0;
  static struct lineInfo     *currentLine=NULL;
  static struct functionInfo *currentFunction=NULL;
  static int                  seenFunctionStart=FALSE;
  static int                  seenEnd=FALSE;
  static int                  seenModuleStart=FALSE;
  static int                  isDefinitionModule=FALSE;
  static int                  totalLines=0;
  static int                  seenOnlySpaces=TRUE;
  static void                *currentComment = NULL;
  static FILE                *inputFile = NULL;

        void mcflex_mcError           (const char *);
static  void pushLine                 (void);
static  void popLine                  (void);
static  void finishedLine             (void);
static  void resetpos                 (void);
static  void consumeLine              (void);
static  void updatepos                (void);
static  void skippos                  (void);
static  void poperrorskip             (const char *);
static  void endOfComment             (void);
static  void handleDate               (void);
static  void handleLine               (void);
static  void handleFile               (void);
static  void handleFunction           (void);
static  void handleColumn             (void);
static  void pushFunction             (char *function, int module);
static  void popFunction              (void);
static  void checkFunction            (void);
        int  mcflex_getColumnNo       (void);
	int  mcflex_openSource        (char *s);
	int  mcflex_getLineNo         (void);
	void mcflex_closeSource       (void);
	char *mcflex_getToken         (void);
        void _M2_mcflex_init          (void);
        int  mcflex_getTotalLines     (void);
extern  void  yylex                   (void);

#if !defined(TRUE)
#    define TRUE  (1==1)
#endif
#if !defined(FALSE)
#    define FALSE (1==0)
#endif

#define YY_DECL void yylex (void)
%}

%x COMMENT COMMENT1 LINE0 LINE1 LINE2

%%

"(*"                       { updatepos();
                             commentLevel=1; pushLine(); skippos(); currentComment = mcComment_initComment (seenOnlySpaces);
			     BEGIN COMMENT; }
<COMMENT>"*)"              { endOfComment(); }
<COMMENT>"(*"              { commentLevel++; pushLine(); updatepos(); skippos(); }
<COMMENT>"<*"              { if (commentLevel == 1) {
                               updatepos();
                               pushLine();
                               skippos();
                               BEGIN COMMENT1;
                             } else
                               updatepos(); skippos();
                           }
<COMMENT>\n                { mcComment_addText (currentComment, yytext); consumeLine(); }
<COMMENT>.                 { mcComment_addText (currentComment, yytext); updatepos(); skippos(); }
<COMMENT1>.                { updatepos(); skippos(); }
<COMMENT1>"*>"             { updatepos(); skippos(); finishedLine(); BEGIN COMMENT; }
<COMMENT1>\n.*             { consumeLine(); }
<COMMENT1>"*)"             { poperrorskip("unterminated source code directive, missing *>");
                             endOfComment(); }
<COMMENT1><<EOF>>          { poperrorskip("unterminated source code directive, missing *>"); BEGIN COMMENT; }
<COMMENT><<EOF>>           { poperrorskip("unterminated comment found at the end of the file, missing *)"); BEGIN INITIAL; }

^\#.*                      { consumeLine(); /* printf("found: %s\n", currentLine->linebuf); */ BEGIN LINE0; }
\n\#.*                     { consumeLine(); /* printf("found: %s\n", currentLine->linebuf); */ BEGIN LINE0; }
<LINE0>\#[ \t]*            { updatepos(); }
<LINE0>[0-9]+[ \t]*\"      { updatepos(); lineno=atoi(yytext)-1; BEGIN LINE1; }
<LINE0>\n                  { mcflex_mcError("missing initial quote after #line directive"); resetpos(); BEGIN INITIAL; }
<LINE0>[^\n]
<LINE1>[^\"\n]+            { mcflex_mcError("missing final quote after #line directive"); resetpos(); BEGIN INITIAL; }
<LINE1>.*\"                { updatepos();
                             filename = (char *)realloc(filename, yyleng+1);
			     strcpy(filename, yytext);
                             filename[yyleng-1] = (char)0;  /* remove trailing quote */
                             START_FILE (filename, lineno);
                             BEGIN LINE2;
                           }
<LINE2>[ \t]*              { updatepos(); }
<LINE2>\n                  { mcLexBuf_setFile(filename); updatepos(); BEGIN INITIAL; }
<LINE2>2[ \t]*\n           { mcLexBuf_setFile(filename); updatepos(); BEGIN INITIAL; }
<LINE2>1[ \t]*\n           { mcLexBuf_setFile(filename); updatepos(); BEGIN INITIAL; }

\n[^\#].*                  { consumeLine(); /* printf("found: %s\n", currentLine->linebuf); */ }
\n                         { consumeLine(); /* printf("found: %s\n", currentLine->linebuf); */ }

\"[^\"\n]*\"               { updatepos(); mcLexBuf_addTokCharStar(mcReserved_stringtok, yytext); return; }
\"[^\"\n]*$                { updatepos();
                             mcflex_mcError("missing terminating quote, \"");
                             resetpos(); return;
                           }

'[^'\n]*'                  { updatepos(); mcLexBuf_addTokCharStar(mcReserved_stringtok, yytext); return; }
'[^'\n]*$                  { updatepos();
                             mcflex_mcError("missing terminating quote, '");
                             resetpos(); return;
                           }

<<EOF>>                    { updatepos(); mcLexBuf_addTok(mcReserved_eoftok); return; }
\+                         { updatepos(); mcLexBuf_addTok(mcReserved_plustok); return; }
-                          { updatepos(); mcLexBuf_addTok(mcReserved_minustok); return; }
"*"                        { updatepos(); mcLexBuf_addTok(mcReserved_timestok); return; }
\/                         { updatepos(); mcLexBuf_addTok(mcReserved_dividetok); return; }
:=                         { updatepos(); mcLexBuf_addTok(mcReserved_becomestok); return; }
\&                         { updatepos(); mcLexBuf_addTok(mcReserved_ambersandtok); return; }
\.                         { updatepos(); mcLexBuf_addTok(mcReserved_periodtok); return; }
\,                         { updatepos(); mcLexBuf_addTok(mcReserved_commatok); return; }
\;                         { updatepos(); mcLexBuf_addTok(mcReserved_semicolontok); return; }
\(                         { updatepos(); mcLexBuf_addTok(mcReserved_lparatok); return; }
\)                         { updatepos(); mcLexBuf_addTok(mcReserved_rparatok); return; }
\[                         { updatepos(); mcLexBuf_addTok(mcReserved_lsbratok); return; }
\]                         { updatepos(); mcLexBuf_addTok(mcReserved_rsbratok); return; }
\(\!                       { updatepos(); mcLexBuf_addTok(mcReserved_lsbratok); return; }
\!\)                       { updatepos(); mcLexBuf_addTok(mcReserved_rsbratok); return; }
\^                         { updatepos(); mcLexBuf_addTok(mcReserved_uparrowtok); return; }
\@                         { updatepos(); mcLexBuf_addTok(mcReserved_uparrowtok); return; }
\{                         { updatepos(); mcLexBuf_addTok(mcReserved_lcbratok); return; }
\}                         { updatepos(); mcLexBuf_addTok(mcReserved_rcbratok); return; }
\(\:                       { updatepos(); mcLexBuf_addTok(mcReserved_lcbratok); return; }
\:\)                       { updatepos(); mcLexBuf_addTok(mcReserved_rcbratok); return; }
\'                         { updatepos(); mcLexBuf_addTok(mcReserved_singlequotetok); return; }
\=                         { updatepos(); mcLexBuf_addTok(mcReserved_equaltok); return; }
\#                         { updatepos(); mcLexBuf_addTok(mcReserved_hashtok); return; }
\<                         { updatepos(); mcLexBuf_addTok(mcReserved_lesstok); return; }
\>                         { updatepos(); mcLexBuf_addTok(mcReserved_greatertok); return; }
\<\>                       { updatepos(); mcLexBuf_addTok(mcReserved_lessgreatertok); return; }
\<\=                       { updatepos(); mcLexBuf_addTok(mcReserved_lessequaltok); return; }
\>\=                       { updatepos(); mcLexBuf_addTok(mcReserved_greaterequaltok); return; }
"<*"                       { updatepos(); mcLexBuf_addTok(mcReserved_ldirectivetok); return; }
"*>"                       { updatepos(); mcLexBuf_addTok(mcReserved_rdirectivetok); return; }
\.\.                       { updatepos(); mcLexBuf_addTok(mcReserved_periodperiodtok); return; }
\.\.\.                     { updatepos(); mcLexBuf_addTok(mcReserved_periodperiodperiodtok); return; }
\:                         { updatepos(); mcLexBuf_addTok(mcReserved_colontok); return; }
\"                         { updatepos(); mcLexBuf_addTok(mcReserved_doublequotestok); return; }
\|                         { updatepos(); mcLexBuf_addTok(mcReserved_bartok); return; }
\!                         { updatepos(); mcLexBuf_addTok(mcReserved_bartok); return; }
\~                         { updatepos(); mcLexBuf_addTok(mcReserved_nottok); return; }
AND                        { updatepos(); mcLexBuf_addTok(mcReserved_andtok); return; }
ARRAY                      { updatepos(); mcLexBuf_addTok(mcReserved_arraytok); return; }
BEGIN                      { updatepos(); mcLexBuf_addTok(mcReserved_begintok); return; }
BY                         { updatepos(); mcLexBuf_addTok(mcReserved_bytok); return; }
CASE                       { updatepos(); mcLexBuf_addTok(mcReserved_casetok); return; }
CONST                      { updatepos(); mcLexBuf_addTok(mcReserved_consttok); return; }
DEFINITION                 { updatepos(); isDefinitionModule = TRUE;
                             mcLexBuf_addTok(mcReserved_definitiontok); return; }
DIV                        { updatepos(); mcLexBuf_addTok(mcReserved_divtok); return; }
DO                         { updatepos(); mcLexBuf_addTok(mcReserved_dotok); return; }
ELSE                       { updatepos(); mcLexBuf_addTok(mcReserved_elsetok); return; }
ELSIF                      { updatepos(); mcLexBuf_addTok(mcReserved_elsiftok); return; }
END                        { updatepos(); seenEnd=TRUE;
                             mcLexBuf_addTok(mcReserved_endtok); return; }
EXCEPT                     { updatepos(); mcLexBuf_addTok(mcReserved_excepttok); return; }
EXIT                       { updatepos(); mcLexBuf_addTok(mcReserved_exittok); return; }
EXPORT                     { updatepos(); mcLexBuf_addTok(mcReserved_exporttok); return; }
FINALLY                    { updatepos(); mcLexBuf_addTok(mcReserved_finallytok); return; }
FOR                        { updatepos(); mcLexBuf_addTok(mcReserved_fortok); return; }
FROM                       { updatepos(); mcLexBuf_addTok(mcReserved_fromtok); return; }
IF                         { updatepos(); mcLexBuf_addTok(mcReserved_iftok); return; }
IMPLEMENTATION             { updatepos(); mcLexBuf_addTok(mcReserved_implementationtok); return; }
IMPORT                     { updatepos(); mcLexBuf_addTok(mcReserved_importtok); return; }
IN                         { updatepos(); mcLexBuf_addTok(mcReserved_intok); return; }
LOOP                       { updatepos(); mcLexBuf_addTok(mcReserved_looptok); return; }
MOD                        { updatepos(); mcLexBuf_addTok(mcReserved_modtok); return; }
MODULE                     { updatepos(); seenModuleStart=TRUE;
                             mcLexBuf_addTok(mcReserved_moduletok); return; }
NOT                        { updatepos(); mcLexBuf_addTok(mcReserved_nottok); return; }
OF                         { updatepos(); mcLexBuf_addTok(mcReserved_oftok); return; }
OR                         { updatepos(); mcLexBuf_addTok(mcReserved_ortok); return; }
PACKEDSET                  { updatepos(); mcLexBuf_addTok(mcReserved_packedsettok); return; }
POINTER                    { updatepos(); mcLexBuf_addTok(mcReserved_pointertok); return; }
PROCEDURE                  { updatepos(); seenFunctionStart=TRUE;
                             mcLexBuf_addTok(mcReserved_proceduretok); return; }
QUALIFIED                  { updatepos(); mcLexBuf_addTok(mcReserved_qualifiedtok); return; }
UNQUALIFIED                { updatepos(); mcLexBuf_addTok(mcReserved_unqualifiedtok); return; }
RECORD                     { updatepos(); mcLexBuf_addTok(mcReserved_recordtok); return; }
REM                        { updatepos(); mcLexBuf_addTok(mcReserved_remtok); return; }
REPEAT                     { updatepos(); mcLexBuf_addTok(mcReserved_repeattok); return; }
RETRY                      { updatepos(); mcLexBuf_addTok(mcReserved_retrytok); return; }
RETURN                     { updatepos(); mcLexBuf_addTok(mcReserved_returntok); return; }
SET                        { updatepos(); mcLexBuf_addTok(mcReserved_settok); return; }
THEN                       { updatepos(); mcLexBuf_addTok(mcReserved_thentok); return; }
TO                         { updatepos(); mcLexBuf_addTok(mcReserved_totok); return; }
TYPE                       { updatepos(); mcLexBuf_addTok(mcReserved_typetok); return; }
UNTIL                      { updatepos(); mcLexBuf_addTok(mcReserved_untiltok); return; }
VAR                        { updatepos(); mcLexBuf_addTok(mcReserved_vartok); return; }
WHILE                      { updatepos(); mcLexBuf_addTok(mcReserved_whiletok); return; }
WITH                       { updatepos(); mcLexBuf_addTok(mcReserved_withtok); return; }
ASM                        { updatepos(); mcLexBuf_addTok(mcReserved_asmtok); return; }
VOLATILE                   { updatepos(); mcLexBuf_addTok(mcReserved_volatiletok); return; }
\_\_DATE\_\_               { updatepos(); handleDate(); return; }
\_\_LINE\_\_               { updatepos(); handleLine(); return; }
\_\_FILE\_\_               { updatepos(); handleFile(); return; }
\_\_FUNCTION\_\_           { updatepos(); handleFunction(); return; }
\_\_COLUMN\_\_             { updatepos(); handleColumn(); return; }
\_\_ATTRIBUTE\_\_          { updatepos(); mcLexBuf_addTok(mcReserved_attributetok); return; }
\_\_BUILTIN\_\_            { updatepos(); mcLexBuf_addTok(mcReserved_builtintok); return; }
\_\_INLINE\_\_             { updatepos(); mcLexBuf_addTok(mcReserved_inlinetok); return; }


(([0-9]*\.[0-9]+)(E[+-]?[0-9]+)?) { updatepos(); mcLexBuf_addTokCharStar(mcReserved_realtok, yytext); return; }
[a-zA-Z_][a-zA-Z0-9_]*     { checkFunction(); updatepos(); mcLexBuf_addTokCharStar(mcReserved_identtok, yytext); return; }
[0-9]+                     { updatepos(); mcLexBuf_addTokCharStar(mcReserved_integertok, yytext); return; }
[0-9]+B                    { updatepos(); mcLexBuf_addTokCharStar(mcReserved_integertok, yytext); return; }
[0-9]+C                    { updatepos(); mcLexBuf_addTokCharStar(mcReserved_integertok, yytext); return; }
[0-9A-F]+H                 { updatepos(); mcLexBuf_addTokCharStar(mcReserved_integertok, yytext); return; }
[\t\r ]+                   { currentLine->tokenpos += yyleng;  /* ignore whitespace */; }
.                          { updatepos(); mcflex_mcError("unrecognised symbol"); skippos(); }

%%

/* Hand built routines.  */

/* handleFile handles the __FILE__ construct by wraping it in double quotes and putting
   it into the token buffer as a string.  */

static void
handleFile (void)
{
  char *s = (char *)alloca (strlen (filename) + 2 + 1);

  strcpy (s, "\"");
  strcat (s, filename);
  strcat (s, "\"");
  mcLexBuf_addTokCharStar (mcReserved_stringtok, s);
}

/* handleLine handles the __LINE__ construct by passing an integer to
   the token buffer.  */

static void
handleLine (void)
{
  mcLexBuf_addTokInteger(mcReserved_integertok, lineno);
}

/* handleColumn handles the __COLUMN__ construct by passing an integer to
   the token buffer.  */

static void
handleColumn (void)
{
  mcLexBuf_addTokInteger(mcReserved_integertok, mcflex_getColumnNo());
}

/* handleDate handles the __DATE__ construct by passing the date
   as a string to the token buffer.  */

static void
handleDate (void)
{
  time_t  clock = time ((time_t *)0);
  char   *sdate = ctime (&clock);
  char   *s     = (char *)alloca (strlen (sdate)+2+1);
  char   *p     = strchr(sdate, '\n');

  if (p != NULL) {
    *p = (char) 0;
  }
  strcpy (s, "\"");
  strcat (s, sdate);
  strcat (s, "\"");
  mcLexBuf_addTokCharStar (mcReserved_stringtok, s);
}

/* handleFunction handles the __FUNCTION__ construct by wrapping
   it in double quotes and putting it into the token buffer as a string.  */

static void
handleFunction (void)
{
  if (currentFunction == NULL)
    mcLexBuf_addTokCharStar (mcReserved_stringtok, (char *)("\"\""));
  else if (currentFunction->module)
    {
      char *s = (char *) alloca (strlen (yytext) +
				 strlen ("\"module  initialization\"") + 1);
      strcpy (s, "\"module ");
      strcat (s, currentFunction->name);
      strcat (s, " initialization\"");
      mcLexBuf_addTokCharStar (mcReserved_stringtok, s);
    }
  else
    {
      char *function = currentFunction->name;
      char *s = (char *)alloca (strlen (function) + 2 + 1);
      strcpy (s, "\"");
      strcat (s, function);
      strcat (s, "\"");
      mcLexBuf_addTokCharStar (mcReserved_stringtok, s);
    }
}

/* pushFunction pushes the function name onto the stack.  */

static void
pushFunction (char *function, int module)
{
  if (currentFunction == NULL)
    {
      currentFunction = (struct functionInfo *)malloc (sizeof (struct functionInfo));
      currentFunction->name = strdup (function);
      currentFunction->next = NULL;
      currentFunction->module = module;
    }
  else
    {
      struct functionInfo *f = (struct functionInfo *)malloc (sizeof (struct functionInfo));
      f->name = strdup (function);
      f->next = currentFunction;
      f->module = module;
      currentFunction = f;
    }
}

/* popFunction pops the current function.  */

static void
popFunction (void)
{
  if (currentFunction != NULL && currentFunction->next != NULL)
    {
      struct functionInfo *f = currentFunction;

      currentFunction = currentFunction->next;
      if (f->name != NULL)
	free (f->name);
      free (f);
    }
}

/* endOfComment - handles the end of comment.  */

static void
endOfComment (void)
{
  if (commentLevel == 1) {
    mcLexBuf_addTokComment (mcReserved_commenttok, currentComment);
  }
  commentLevel--;
  updatepos ();
  skippos ();
  if (commentLevel==0) {
    BEGIN INITIAL;
    finishedLine ();
  } else
    popLine ();
}

/* mcflex_mcError displays the error message s after the code line and pointer
   to the erroneous token.  */

void
mcflex_mcError (const char *s)
{
  if (currentLine->linebuf != NULL) {
    int i=1;

    printf("%s:%d:%s\n", filename, currentLine->actualline, currentLine->linebuf);
    printf("%s:%d:%*s", filename, currentLine->actualline, 1+currentLine->tokenpos, "^");
    while (i<currentLine->toklen) {
      putchar('^');
      i++;
    }
    putchar('\n');
  }
  printf("%s:%d:%s\n", filename, currentLine->actualline, s);
}

static void
poperrorskip (const char *s)
{
  int nextpos =currentLine->nextpos;
  int tokenpos=currentLine->tokenpos;

  popLine();
  mcflex_mcError(s);
  if (currentLine != NULL) {
    currentLine->nextpos  = nextpos;
    currentLine->tokenpos = tokenpos;
  }
}

/* consumeLine reads a line into a buffer, it then pushes back the whole
   line except the initial \n.  */

static void
consumeLine (void)
{
  if (currentLine->linelen<yyleng) {
    currentLine->linebuf = (char *)realloc (currentLine->linebuf, yyleng);
    currentLine->linelen = yyleng;
  }
  strcpy(currentLine->linebuf, yytext+1);  /* Copy all except the initial \n */
  lineno++;
  totalLines++;
  currentLine->actualline = lineno;
  currentLine->tokenpos=0;
  currentLine->nextpos=0;
  currentLine->column=0;
  START_LINE (lineno, yyleng);
  yyless(1);                  /* Push back all but the \n */
  seenOnlySpaces=TRUE;
}

/* detectSpaces scan yytext to see if only spaces have been seen.  */

static void
detectSpaces (void)
{
  char *p = yytext;
  int   i = 0;

  if ((strcmp (yytext, "(*") != 0) &&
      (strcmp (yytext, "*)") != 0))
    {
      while (i < strlen (p))
        {
          if (! isspace (p[i]))
            seenOnlySpaces = FALSE;
          else if (p[i] == '\n')
            seenOnlySpaces = TRUE;
          i++;
        }
    }
}

/* updatepos updates the current token position.
   Should be used when a rule matches a token.  */

static void
updatepos (void)
{
  seenFunctionStart    = FALSE;
  seenEnd              = FALSE;
  seenModuleStart      = FALSE;
  currentLine->nextpos = currentLine->tokenpos+yyleng;
  currentLine->toklen  = yyleng;
  if (currentLine->column == 0)
    currentLine->column = currentLine->tokenpos;
  if (commentLevel == 0)
    detectSpaces ();
}

/* checkFunction checks to see whether we have seen the start
   or end of a function.  */

static void
checkFunction (void)
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
  seenFunctionStart = FALSE;
  seenEnd           = FALSE;
  seenModuleStart   = FALSE;
}

/* skippos skips over this token. This function should be called
   if we are not returning and thus not calling getToken.  */

static void
skippos (void)
{
  currentLine->tokenpos = currentLine->nextpos;
}

/* initLine initializes a currentLine.  */

static void initLine (void)
{
  currentLine = (struct lineInfo *)malloc (sizeof(struct lineInfo));

  if (currentLine == NULL)
    perror("malloc");
  currentLine->linebuf    = NULL;
  currentLine->linelen    = 0;
  currentLine->tokenpos   = 0;
  currentLine->toklen     = 0;
  currentLine->nextpos    = 0;
  currentLine->actualline = lineno;
  currentLine->column     = 0;
  currentLine->inuse      = TRUE;
  currentLine->next       = NULL;
}

/* pushLine pushes a new line structure.  */

static void
pushLine (void)
{
  if (currentLine == NULL)
    initLine();
  else if (currentLine->inuse) {
      struct lineInfo *l = (struct lineInfo *)malloc (sizeof(struct lineInfo));

      if (currentLine->linebuf == NULL) {
	l->linebuf  = NULL;
	l->linelen  = 0;
      } else {
	l->linebuf    = (char *)strdup (currentLine->linebuf);
	l->linelen    = strlen (l->linebuf)+1;
      }
      l->tokenpos   = currentLine->tokenpos;
      l->toklen     = currentLine->toklen;
      l->nextpos    = currentLine->nextpos;
      l->actualline = currentLine->actualline;
      l->column     = currentLine->column;
      l->next       = currentLine;
      currentLine   = l;
  }
  currentLine->inuse = TRUE;
}

/* popLine pops a line structure.  */

static void
popLine (void)
{
  if (currentLine != NULL) {
    struct lineInfo *l = currentLine;

    if (currentLine->linebuf != NULL)
      free(currentLine->linebuf);
    currentLine = l->next;
    free(l);
  }
}

/* resetpos resets the position of the next token to the start of the line.  */

static void
resetpos (void)
{
  if (currentLine != NULL)
    currentLine->nextpos = 0;
}

/* finishedLine indicates that the current line does not need to be preserved
   when a pushLine occurs.  */

static void
finishedLine (void)
{
  currentLine->inuse = FALSE;
}

/* mcflex_getToken returns a new token.  */

char *
mcflex_getToken (void)
{
  TIMEVAR_PUSH_LEX;
  if (currentLine == NULL)
    initLine ();
  currentLine->tokenpos = currentLine->nextpos;
  yylex ();
  TIMEVAR_POP_LEX;
  return yytext;
}

/* closeSource - provided for semantic sugar.  */

void
mcflex_closeSource (void)
{
  END_FILE ();
}

/* openSource returns TRUE if file s can be opened and
   all tokens are taken from this file.  */

int
mcflex_openSource (char *s)
{
  FILE *newInputFile = fopen (s, "r");

  if (newInputFile == NULL)
    return FALSE;
  else
    {
      isDefinitionModule = FALSE;
      while (currentFunction != NULL)
	{
	  struct functionInfo *f = currentFunction;
	  currentFunction = f->next;
	  if (f->name != NULL)
	    free (f->name);
	  free (f);
	}
      yy_delete_buffer (YY_CURRENT_BUFFER);
      if (inputFile != NULL)
	fclose (inputFile);
      inputFile = newInputFile;
      yy_switch_to_buffer (yy_create_buffer (inputFile, YY_BUF_SIZE));
      filename = strdup (s);
      lineno =1;
      if (currentLine != NULL)
	currentLine->actualline = lineno;
      START_FILE (filename, lineno);
      return TRUE;
    }
}

/* mcflex_getLineNo returns the current line number.  */

int
mcflex_getLineNo (void)
{
  if (currentLine != NULL)
    return currentLine->actualline;
  else
    return 0;
}

/* mcflex_getColumnNo returns the column where the current
   token starts.  */

int
mcflex_getColumnNo (void)
{
  if (currentLine != NULL)
    return currentLine->column;
  else
    return 0;
}

/* getTotalLines returns the total number of lines parsed.  */

int
mcflex_getTotalLines (void)
{
  return totalLines;
}

/* yywrap is called when end of file is seen.  We push an eof token
   and tell the lexical analysis to stop.  */

int
yywrap (void)
{
  updatepos ();
  mcLexBuf_addTok (mcReserved_eoftok);
  return 1;
}

void
_M2_mcflex_init (void)
{
}

void
_M2_mcflex_fini (void)
{
}

void
_M2_mcflex_finish (void)
{
}

/* This is a gross hack to satisfy linking.  */

void
_M2_mcflex_ctor (void)
{
}
