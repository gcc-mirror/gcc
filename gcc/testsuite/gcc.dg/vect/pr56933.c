/* { dg-require-effective-target vect_double } */

#include "tree-vect.h"

extern void abort (void);
void __attribute__((noinline,noclone))
foo (double *b, double *d, double *f)
{
  int i;
  for (i = 0; i < 1024; i++)
    {
      d[2*i] = 2. * d[2*i];
      d[2*i+1] = 4. * d[2*i+1];
      b[i] = d[2*i] - 1.;
      f[i] = d[2*i+1] + 2.;
    }
}
int main()
{
  double b[1024], d[2*1024], f[1024];
  int i;

  check_vect ();

  for (i = 0; i < 2*1024; i++)
    d[i] = 1.;
  foo (b, d, f);
#pragma GCC novector
  for (i = 0; i < 1024; i+= 2)
    {
      if (d[2*i] != 2.)
	abort ();
      if (d[2*i+1] != 4.)
	abort ();
    }
#pragma GCC novector
  for (i = 0; i < 1024; i++)
    {
      if (b[i] != 1.)
	abort ();
      if (f[i] != 6.)
	abort ();
    }
  return 0;
}

