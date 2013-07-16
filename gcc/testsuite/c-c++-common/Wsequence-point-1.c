/* PR c++/57274 */
/* { dg-do compile } */
/* { dg-options "-Wsequence-point" } */

void foo (int, int);

void
bar (int *x)
{
  foo (*x++, sizeof (*x));	/* { dg-bogus "may be undefined" } */
}

void
baz (int *x)
{
  foo (*x, sizeof (*x++) + sizeof (*x++));	/* { dg-bogus "may be undefined" } */
}
