/* PR c/35441 */
/* { dg-do compile } */
/* { dg-options "-fno-diagnostics-show-caret" } */
/* { dg-bogus "not supported by" "" { target *-*-* } 0 } */

void foo1(char **p, char **q)
{
  (p - q)();			/* { dg-error "is not a function" } */
}

void foo2(double x, double y)
{
  (x/y)();			/* { dg-error "is not a function" } */
}

void foo3(unsigned i, int j)
{
  (i << j | i >> (32 - j))();	/* { dg-error "is not a function" } */
  (i >> j | i << (32 - j))();	/* { dg-error "is not a function" } */
}

void foo4(char *p, char *q)
{
  (p < q ? p : q)();		/* { dg-error "is not a function" } */
  (p > q ? p : q)();		/* { dg-error "is not a function" } */
}
