/* PR tree-optimization/59924 */
/* { dg-do compile } */
/* { dg-options "-O1 -Wall" } */

struct S { struct T *a; double b; struct S *c; };
struct T { struct S *d; };
extern void bar (double);

void
foo (struct S * x, int y, int z, int w)
{
  int e;
  struct S *f;
  for (f = x->a->d; f; f = f->c)
    {
      if (5 < w)
	{
	  e = -w;
	  z = w;
	}
    }
  if (y != 0 || z != 0)
    {
      double g = x->b + (double) e * (double) y; /* { dg-warning "may be used uninitialized in this function" } */
      bar (g * g);
    }
}
