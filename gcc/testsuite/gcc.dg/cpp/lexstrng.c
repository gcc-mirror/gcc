/* Copyright (C) 2000 Free Software Foundation, Inc.  */

/* { dg-do run } */
/* { dg-options "-trigraphs" } */

/* Test lexing of strings and character constants.  */

#ifndef __WCHAR_TYPE__
#define __WCHAR_TYPE__ int
#endif
typedef __WCHAR_TYPE__ wchar_t;

extern int strcmp (const char *, const char *);
#if DEBUG
extern int puts (const char *);
#else
#define puts(X)
#endif
extern void abort (void);
#define err(str) do { puts(str); abort(); } while (0)

/* Escaped newlines.  */
const char *str1 = "s\
t\
\
r??/
  1";

const char x = '\
??/
b';

/* Test escaped terminators.  */
const char *term = "\"\\\"\\";
const char termc = '\'';
const char *terms = "'";

/* Test wide strings and chars are lexed.  */
const wchar_t wchar = L'w';
const wchar_t* wstring = L"wide string";

/* Test all 9 trigraphs embedded in a string.  Test trigraphs do not
   survive an embedded backslash newline.  Test trigraphs preceded by
   a '?' are still noticed.  */
const char *t = "??/\??<??>??=??)??\
(??(??!??'??-???=???/
?-";

int main (int argc, char *argv[])
{
  if (strcmp (str1, "str  1"))
    err ("str1");

  if (x != 'b')
    err ("b");

  /* We have to split the string up to avoid trigraph replacement
     here.  Split the 2 trigraphs after both 1 and 2 ?s; just doing
     this exposed a bug in the initial release of the tokenized lexer.  */
  if (strcmp (t, "\\{}#]?" "?([|^~?#??" "-"))
    err ("Embedded trigraphs");

  if (term[0] != '"' || term[1] != '\\' || term[2] != '"'
      || term[3] != '\\' || term[4] != '\0')
    err ("Escaped string terminators");

  if (termc != terms[0])
    err ("Escaped character constant terminator");

  return 0;
}
