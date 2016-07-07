/* PR tree-optimization/69802 */
/* { dg-do compile } */
/* { dg-options "-O2 -Wall" } */

struct S { unsigned f : 1; };
int a, d;

int
foo (void)
{
  unsigned b = 0;
  struct S c;
  d = ((1 && b) < c.f) & c.f;	/* { dg-warning "is used uninitialized" } */
  return a;
}

int
bar (_Bool c)
{
  unsigned b = 0;
  d = ((1 && b) < c) & c;
  return a;
}
