/* { dg-do run } */
/* PR tree-optimization/126470 */

/* *p should not become unconditional.  */

__attribute__((noipa)) int
f (int a, int *p)
{
  return a != 0 ? a / (*p | 1) : 0;
}

int
main (void)
{
  if (f (0, 0) != 0)
    __builtin_abort ();
  return 0;
}
