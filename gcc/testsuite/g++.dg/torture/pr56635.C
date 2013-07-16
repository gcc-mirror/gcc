// PR tree-optimization/56635
// { dg-do compile }

struct A { _Complex double a; };

void
foo (A **x, A **y)
{
  A r;
  if (__real__ x[0]->a)
    {
      r.a = y[0]->a / x[0]->a;
      **x = r;
    }
  else
    **x = **y;
}
