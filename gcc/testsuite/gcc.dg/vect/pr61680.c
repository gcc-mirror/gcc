/* { dg-do run } */

#include "tree-vect.h"

double v[4096][4];

__attribute__((noinline, noclone)) void
bar (double p[][4])
{
  int i;
  double d = 172.0;
  for (i = 0; i < 4096; i++)
    {
      if (p[i][0] != 6.0 || p[i][1] != 6.0 || p[i][2] != 10.0)
	__builtin_abort ();
      if (__builtin_fabs (p[i][3] - d) > 0.25)
	__builtin_abort ();
    }
}

__attribute__((noinline, noclone)) void
foo (void)
{
  int i;
  double w[4096][4], t;
  for (i = 0; i < 4096; i++)
    {
      w[i][0] = v[i][0] + 2.0;
      w[i][1] = v[i][1] + 1.0;
      w[i][2] = v[i][2] + 4.0;
      w[i][3] = (w[i][0] * w[i][0] + w[i][1] * w[i][1] + w[i][2] * w[i][2]);
    }
  bar (w);
}

int
main ()
{
  int i;

  check_vect ();

  for (i = 0; i < 4096; i++)
    {
      v[i][0] = 4.0;
      v[i][1] = 5.0;
      v[i][2] = 6.0;
    }
  foo ();
  return 0;
}
