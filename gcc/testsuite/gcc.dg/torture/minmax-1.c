/* { dg-do run } */
/* PR tree-optimization/126456 */

/* These should not produce min/max for
   the outer conditional.  */

__attribute__((noipa)) int
min_le (int a, int c)
{
  return (a <= 6) ? (a < c ? a : c) : (5 < c ? 5 : c);
}

__attribute__((noipa)) int
max_ge (int a, int c)
{
  return (a >= 4) ? (a > c ? a : c) : (5 > c ? 5 : c);
}

int
main (void)
{
  if (min_le (6, 10) != 6)
    __builtin_abort ();
  if (max_ge (4, 0) != 4)
    __builtin_abort ();
  return 0;
}

