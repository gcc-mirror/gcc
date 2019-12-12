/* { dg-do run { target sse2_runtime } } */
/* { dg-options "-O2 -msse2 -fexcess-precision=standard" } */

#include "pr88828-7a.c"
#include "pr88828-7b.c"

extern void abort ();

float
bar (float x, float y)
{
  return x / y - y * x;
}

void
do_check (__v4sf x, float f1[4], float f2[4])
{
  int i;

  for (i = 0; i < 4; i++)
    if (i == 0)
      {
	if (x[i] != bar (f1[i], f2[i]))
	  abort ();
      }
    else
      {
	if (x[i] != f1[i])
	  abort ();
      }
}

int
main (void)
{
  float f1[4] = { -11, 2, 55553, -4 };
  float f2[4] = { 111, 3.3, -55.553, 4.8 };
  __v4sf x = { f1[0], f1[1], f1[2], f1[3] };
  __v4sf y = { f2[0], f2[1], f2[2], f2[3] };
  __v4sf z;
  int i;

  for (i = 0; i < 4; i++)
    if (x[i] != f1[i] || y[i] != f2[i] )
      abort ();

  z = foo1 (x, y);
  do_check (z, f1, f2);
  x = foo2 (x, y);
  do_check (z, f1, f2);

  return 0;
}
