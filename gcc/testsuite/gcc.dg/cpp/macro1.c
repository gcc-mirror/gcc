/* Copyright (C) 2000 Free Software Foundation, Inc.  */

/* { dg-do run } */

/* Tests various macros are correctly expanded.  */

extern int puts (const char *);
extern void abort (void);
#define err(str) do { puts(str); abort(); } while (0)

#define j(x, y) x + y
#define k(x, y) j(x + 2, y +

int q(int x)		{return x + 40;}
int B(int x)		{return x + 20;}
int foo(int x)		{return x + 10;}
int bar(int x, int y)	{return x + y;}
int baz(int x, int y)	{return x + y;}
int toupper(int x)	{return x + 32;}

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

#define __tobody(c, f) f (c)
#define toupper(c) __tobody (c, toupper)
  if (toupper (10) != 42)	/* toupper (10). */
    err ("toupper");

  /* This looks like it has too many ')', but it hasn't.  */
  if (k(1, 4) 35) != 42)
    err ("k");

    /*#define B(x) Z B(x)
#define XEXP(RTX, N)  RTX->fld[N].rtx
#define PATTERN(INSN) XEXP(INSN, 3)
#define COST(X) XEXP (XEXP (x, 0), 0)
#define M(a) OK M (a)
#define stpcpy(a) M(a)
#define C(x) A(x)
XEXP (PATTERN (insn), i);
XEXP (XEXP (insn, 3), i);
COST (b)*/

  return 0;
}
