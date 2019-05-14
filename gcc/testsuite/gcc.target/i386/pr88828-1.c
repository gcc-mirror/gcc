/* { dg-do run { target sse2_runtime } } */
/* { dg-options "-O2 -msse2" } */

#include "pr88828-1a.c"
#include "pr88828-1b.c"
#include "pr88828-1c.c"

extern void abort ();

void
do_check (__v4sf y, float f[4], float z)
{
  int i;

  for (i = 0; i < 4; i++)
    if (i == 0)
      {
	if (y[i] != z)
	  abort ();
      }
    else
      {
	if (y[i] != f[i])
	  abort ();
      }
}

int
main (void)
{
  float f[4] = { -11, 2, 55553, -4 };
  float z = 134567;
  __v4sf x = { f[0], f[1], f[2], f[3] };
  __v4sf y;
  int i;

  for (i = 0; i < 4; i++)
    if (x[i] != f[i])
      abort ();

  y = foo1 (x, z);
  do_check (y, f, z);
  y = foo2 (x, z);
  do_check (y, f, z);
  y = foo3 (x, z);
  do_check (y, f, z);

  return 0;
}
