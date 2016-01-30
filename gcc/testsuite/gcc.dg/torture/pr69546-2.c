/* PR tree-optimization/69546 */
/* { dg-do run { target int128 } } */

unsigned __int128
foo (void)
{
  unsigned __int128 a = 0xfffffffffffffffeULL;
  unsigned __int128 b = 0xffffffffffffffffULL;
  return a % b;
}

int
main ()
{
  if (foo () != 0xfffffffffffffffeULL)
    __builtin_abort ();
  return 0;
}
