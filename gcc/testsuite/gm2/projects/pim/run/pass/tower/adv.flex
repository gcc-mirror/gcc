%{
/* Copyright (C) 2022 Free Software Foundation, Inc.
   This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GNU Modula-2 is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License along
with gm2; see the file COPYING.  If not, write to the Free Software
Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */

  /*
   *  adv.flex - provides a lexical analyser for Dungeon
   */

  struct lineInfo {
    char            *linebuf;          /* line contents */
    int              linelen;          /* length */
    int              tokenpos;         /* start position of token within line */
    int              toklen;           /* a copy of yylen (length of token) */
    int              nextpos;          /* position after token */
    int              actualline;       /* line number of this line */
  };

  static int                  lineno      =1;   /* a running count of the file line number */
  static char                *filename    =NULL;
  static struct lineInfo     *currentLine =NULL;

        void advflex_error      (const char *);
static  void finishedLine       (void);
static  void resetpos           (void);
static  void consumeLine        (void);
static  void updatepos          (void);
static  void skippos            (void);
static  void poperrorskip       (const char *);
	int  advflex_OpenSource (char *s);
	int  advflex_GetLineNo  (void);
	void advflex_CloseSource(void);
	char *advflex_GetToken  (void);
        void _M2_advflex_init   (int, char *, char *);
        void _M2_advflex_finish (int, char *, char *);
        void _M2_advflex_ctor   (void);
extern  void  yylex             (void);

#if !defined(TRUE)
#    define TRUE  (1==1)
#endif
#if !defined(FALSE)
#    define FALSE (1==0)
#endif

typedef enum {eoftok, roomtok, doortok, walltok, treasuretok, attok,
              leadstok, totok, statustok, closedtok, opentok, secrettok,
              istok, endtok, enddottok, integertok, randomizetok} toktype ;

toktype  advflex_currenttoken;
char    *advflex_currentident;
int      advflex_currentinteger;


#define YY_DECL void yylex (void)
%}

%%

\n.*                      { consumeLine(); /* printf("found: %s\n", currentLine->linebuf); */ }
[0-9]+                    { updatepos();
                            advflex_currenttoken = integertok;
                            advflex_currentinteger = atoi(yytext);
                            return; }
[ \t]*                    { updatepos(); }
ROOM                      { updatepos(); advflex_currenttoken = roomtok; return; }
END                       { updatepos(); advflex_currenttoken = endtok; return; }
WALL                      { updatepos(); advflex_currenttoken = walltok; return; }
DOOR                      { updatepos(); advflex_currenttoken = doortok; return; }
STATUS                    { updatepos(); advflex_currenttoken = statustok; return; }
CLOSED                    { updatepos(); advflex_currenttoken = closedtok; return; }
OPEN                      { updatepos(); advflex_currenttoken = opentok; return; }
SECRET                    { updatepos(); advflex_currenttoken = secrettok; return; }
LEADS                     { updatepos(); advflex_currenttoken = leadstok; return; }
TO                        { updatepos(); advflex_currenttoken = totok; return; }
TREASURE                  { updatepos(); advflex_currenttoken = treasuretok; return; }
AT                        { updatepos(); advflex_currenttoken = attok; return; }
IS                        { updatepos(); advflex_currenttoken = istok; return; }
END.                      { updatepos(); advflex_currenttoken = enddottok; return; }
RANDOMIZE                 { updatepos(); advflex_currenttoken = randomizetok; return; }
<<EOF>>                   { updatepos(); advflex_currenttoken = eoftok; return; }

%%

/*
 *  consumeLine - reads a line into a buffer, it then pushes back the whole
 *                line except the initial \n.
 */

static void consumeLine (void)
{
  if (currentLine->linelen<yyleng) {
    currentLine->linebuf = (char *)realloc (currentLine->linebuf, yyleng);
    currentLine->linelen = yyleng;
  }
  strcpy(currentLine->linebuf, yytext+1);  /* copy all except the initial \n */
  lineno++;
  currentLine->actualline = lineno;
  currentLine->tokenpos=0;
  currentLine->nextpos=0;
  yyless(1);                  /* push back all but the \n */
}

/*
 *  updatepos - updates the current token position.
 *              Should be used when a rule matches a token.
 */

static void updatepos (void)
{
  currentLine->nextpos = currentLine->tokenpos+yyleng;
  currentLine->toklen  = yyleng;
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
  currentLine = (struct lineInfo *)malloc (sizeof(struct lineInfo));

  if (currentLine == NULL)
    perror("malloc");
  currentLine->linebuf    = NULL;
  currentLine->linelen    = 0;
  currentLine->tokenpos   = 0;
  currentLine->toklen     = 0;
  currentLine->nextpos    = 0;
  currentLine->actualline = lineno;
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
 *  advflex_GetToken - returns a new token.
 */

char *advflex_GetToken (void)
{
  if (currentLine == NULL)
    initLine();
  currentLine->tokenpos = currentLine->nextpos;
  yylex();
}

void advflex_error (const char *s)
{
  if (currentLine != NULL) {
    printf("%s:%d:%s\n", filename, currentLine->actualline, s);
    printf("%s\n", currentLine->linebuf);
# if 0
    printf("%*s%*s\n", currentLine->nextpos, " ", currentLine->toklen, "^");
# endif
  }
}

/*
 *  OpenSource - returns TRUE if file, s, can be opened and
 *               all tokens are taken from this file.
 */

int advflex_OpenSource (char *s)
{
  FILE *f = fopen(s, "r");

  if (f == NULL)
    return FALSE;
  else {
    yy_delete_buffer(YY_CURRENT_BUFFER);
    yy_switch_to_buffer(yy_create_buffer(f, YY_BUF_SIZE));
    filename = strdup(s);
    lineno   =1;
    if (currentLine != NULL)
      currentLine->actualline = lineno;
    return TRUE;
  }
}

/*
 *  CloseSource - provided for semantic sugar
 */

void advflex_CloseSource (void)
{
}

/*
 *  advflex_GetLineNo - returns the current line number.
 */

int advflex_GetLineNo (void)
{
  if (currentLine != NULL)
    return currentLine->actualline;
  else
    return 0;
}

/*
 *  yywrap is called when end of file is seen. We push an eof token
 *         and tell the lexical analysis to stop.
 */

int yywrap (void)
{
  updatepos(); return 1;
}

void _M2_advflex_init (int, char *, char *)
{
}

void _M2_advflex_finish (int, char *, char *)
{
}

void _M2_advflex_ctor (void)
{
}

#if 0
main () {
  char *s;

  if (advflex_OpenSource("../maps/glover")) {
    s = (char *)advflex_GetToken();
    while (s != NULL) {
      advflex_error(s);
      s = (char *)advflex_GetToken();
    }
  }
}
#endif
