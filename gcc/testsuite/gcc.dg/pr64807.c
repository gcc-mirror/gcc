/* PR tree-optimization/64807 */
/* { dg-do run { target int128 } } */
/* { dg-options "-O2" } */

__uint128_t
foo (void)
{
  __uint128_t a = -1;
  __uint128_t b = -1;
  return a / b;
}

int
main ()
{
  if (foo () != 1)
    __builtin_abort ();
  return 0;
}
