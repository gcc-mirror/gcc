/* PR rtl-optimization/85167 */
/* { dg-do compile } */
/* { dg-options "-O2 -w" } */

struct A { long b; };
int c, d, e;
int bar (void);

int
foo (void)
{
  long g;
  for (; g == c ? 0 : (e = 1); g = ((struct A *)g)->b)
    if (bar ())
      return d;
}
