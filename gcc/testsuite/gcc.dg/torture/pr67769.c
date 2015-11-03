/* { dg-do run } */

static int
clamp (int x, int lo, int hi)
{
  return (x < lo) ? lo : ((x > hi) ? hi : x);
}

__attribute__ ((noinline))
short
foo (int N)
{
  short value = clamp (N, 0, 16);
  return value;
}

int
main ()
{
  if (foo (-5) != 0)
    __builtin_abort ();
  return 0;
}
