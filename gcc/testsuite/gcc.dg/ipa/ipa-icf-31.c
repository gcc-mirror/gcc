/* { dg-do run } */
/* { dg-options "-O2 -fipa-icf" } */

__attribute__ ((noinline, noclone))
int f1 (int x, int (*p1) (void), int (*p2) (void))
{
  if (x)
    return p1 ();
  else
    return p2 ();
}

__attribute__ ((noinline, noclone))
int f2 (int x, int (*p1) (void), int (*p2) (void))
{
  if (x)
    return p2 ();
  else
    return p1 ();
}

__attribute__ ((noinline, noclone))
int f3 (void)
{
  return 1;
}

__attribute__ ((noinline, noclone))
int f4 (void)
{
  return 2;
}

int
main ()
{
  if (f1 (0, f3, f4) != 2 || f1 (1, f3, f4) != 1 || f2 (0, f3, f4) != 1
      || f2 (1, f3, f4) != 2)
    __builtin_abort ();
  return 0;
}
