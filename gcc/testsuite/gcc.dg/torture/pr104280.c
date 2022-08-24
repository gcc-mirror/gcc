/* PR tree-optimization/104280 */
/* { dg-do run } */

int
foo (unsigned b, int c)
{
  return b / c;
}

int
main ()
{
  if (foo (1, 2) != 0)
    __builtin_abort ();
  return 0;
}
