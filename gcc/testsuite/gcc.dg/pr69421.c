/* { dg-do compile } */
/* { dg-options "-O3" } */

struct A { double a; };
double a;

void
foo (_Bool *x)
{
  long i;
  for (i = 0; i < 64; i++)
    {
      struct A c;
      x[i] = c.a || a;
    }
}
