/* { dg-do run } */
/* { dg-skip-if "" { *-*-* } { "*" } { "-O2" } } */
/* { dg-options "-fsanitize=undefined -fipa-icf" } */

__attribute__ ((noinline, noclone))
int f1 (int x, int y)
{
  return x + y;
}

__attribute__ ((noinline, noclone))
int f2 (int x, int y)
{
  return x - y;
}

int
main ()
{
  if (f1 (5, 6) != 11 || f2 (5, 6) != -1)
    __builtin_abort ();
  return 0;
}
