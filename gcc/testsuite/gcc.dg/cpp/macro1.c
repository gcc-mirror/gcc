/* Copyright (C) 2000 Free Software Foundation, Inc.  */

/* { dg-do run } */

/* Tests various macro abuse is correctly expanded.  */

extern int puts (const char *);
extern void abort (void);
extern int strcmp(const char *s1, const char *s2);

#define err(str) do { puts(str); abort(); } while (0)
#define j(x, y) x + y
#define k(x, y) j(x + 2, y +
#define glue(x, y) x ## y
#define xglue(x, y) glue(x, y)

/* Functions called when macros are left unexpanded.  */
int q(int x)		{return x + 40;}
int B(int x)		{return x + 20;}
int foo(int x)		{return x + 10;}
int bar(int x, int y)	{return x + y;}
int baz(int x, int y)	{return x + y;}
int toupper(int x)	{return x + 32;}
int M(int x)		{return x * 2;}

int main (int argc, char *argv[])
{
#define q(x) x
  if (q(q)(2) != 42)
    err ("q");

#define A(x) B(x)
  if (A(A(2)) != 42)
    err ("A");

#define E(x) A x
#define F (22)
  if (E(F) != 42)
    err ("E(F)");

#define COMMA ,
#define NASTY(a) j(a 37)
  if (NASTY (5 COMMA) != 42)
    err ("NASTY");

#define bar(x, y) foo(x(y, 0))
#define apply(x, y) foo(x(y, 22))
#define bam bar
  if (bar(bar, 32) != 42)	/* foo(bar(32, 0)).  */
    err ("bar bar");
  if (bar(bam, 32) != 42)	/* Same.  */
    err ("bar bam");
  if (apply(bar, baz) != 42)	/* foo(foo(baz(22, 0))).  */
    err ("apply bar baz");

  /* Taken from glibc.  */
#define __tobody(c, f) f (c)
#define toupper(c) __tobody (c, toupper)
  if (toupper (10) != 42)	/* toupper (10). */
    err ("toupper");

  /* This tests that M gets expanded the right no. of times.  Too many
     times, and we get excess "2 +"s and the wrong sum.  Derived from
     nested stpcpy in dggettext.c.  */
#define M(x) 2 + M(x)
#define stpcpy(a) M(a)
  if (stpcpy (stpcpy (9)) != 42) /*  2 + M (2 + M (9)) */
    err ("stpcpy");

  /* Another test derived from nested stpcpy's of dggettext.c.  Uses
     macro A(x) and function B(x) as defined above.  The problem was
     the same - excess "1 +"s and the wrong sum.  */
#define B(x) 1 + B(x)
#define C(x) A(x)
  if (C(B(0)) != 42)		/* 1 + B (1 + B (0)) */
    err ("C");

  /* More tests derived from gcc itself - the use of XEXP and COST.
     These first two should both expand to the same thing.  */
  {
    int insn = 6, i = 2, b = 2;
#define XEXP(RTX, N)  (RTX * N + 2)
#define PATTERN(INSN) XEXP(INSN, 3)
    if (XEXP (PATTERN (insn), i) != 42)	/* ((insn * 3 + 2) * i + 2) */
      err ("XEXP (PATTERN)");
    if (XEXP (XEXP (insn, 3), i) != 42)	/* ((insn * 3 + 2) * i + 2) */
      err ("XEXP (XEXP)");

#define COST(X) XEXP (XEXP (X, 4), 4)
    if (COST (b) != 42)		/* ((b * 4 + 2) * 4 + 2) */
      err ("COST");
  }

  /* This tests macro recursion and expand-after-paste.  */
#define FORTYTWO "forty"
#define TWO TWO "-two"
  if (strcmp (glue(FORTY, TWO), "forty"))
    err ("glue");
  if (strcmp (xglue(FORTY, TWO), "forty-two"))
    err ("xglue");

  /* Test ability to call macro over multiple logical lines.  */
  if (q
      (42) != 42
      || q (
	 42) != 42
      || q (42
	    ) != 42
      || q
      (
       42
       )
      != 42)
    err ("q over multiple lines");

  /* Corner case.  Test that macro expansion is turned off for later
     q, when not at start but at end of argument context, and supplied
     with the '(' necessary for expansion.  */
  if (q(1 + q)(1) != 42)	/* 1 + q(1) */
    err ("Nested q");

  /* This looks like it has too many ')', but it hasn't.  */
  if (k(1, 4) 35) != 42)
    err ("k");

  /* Phew! */
  return 0;
}
