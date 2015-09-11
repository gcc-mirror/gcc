/* Copyright (C) 2000 Free Software Foundation, Inc.  */

/* { dg-do run } */
/* { dg-options "-trigraphs" } */

/* Test lexing of numbers.  */

#if DEBUG
extern int puts (const char *);
#else
#define puts(X)
#endif
extern void abort (void);
#define err(str) do { puts(str); abort(); } while (0)

/* Escaped newlines.  */
#define foo 12\
3\
\
4??/
5

#if foo != 12345
#error foo
#endif

int main (int argc, char *argv[])
{
  double a = 5.;
  double x = .5;

/* Decimal points, including initially and immediately before and
   after an escaped newline.  */
  if (a != 5)
    err ("a");
  if (x != .\
5)
    err ("x != .5");
  x = 25\
.\
6;
  if (x != 25.6)
    err ("x != 25.6");

  /* Test exponentials and their signs.  A buggy lexer is more likely
     to fail the compile, but never mind.  */
  if (250 != 25e+1 || 250 != 25e1 || 250 != 2500e-1)
    err ("exponentials");

  /* Todo: p exponentials, and how to test preprocessing number
     tokenisation?  */

  return 0;
}
