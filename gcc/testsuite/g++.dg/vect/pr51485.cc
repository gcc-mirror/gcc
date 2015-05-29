/* { dg-do compile } */

struct A { A (); unsigned int a; };
double bar (A a) throw () __attribute__((pure));

void
foo (unsigned int x, double *y, A *z)
{
  unsigned int i;
  for (i = 0; i < x; i++)
    y[i] = bar (z[i]);
}

