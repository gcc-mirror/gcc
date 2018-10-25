/* PR tree-optimization/84436 */
/* { dg-options "-O2 -fdump-tree-switchconv -fdump-tree-optimized" } */
/* { dg-do run } */

signed char
__attribute__ ((noipa))
foo (signed char how)
{
  switch (how) {
    case -4: how = 96; break;
    case -3: how = -120; break;
    case -2: how = -80; break;
    case -1: how = -40; break;
    case 0: how = 0; break;
    case 1: how = 40; break;
  }
  return how;
}

int main()
{
  if (foo (-4) != 96)
  __builtin_abort ();

  if (foo (-3) != -120)
  __builtin_abort ();

  if (foo (0) != 0)
  __builtin_abort ();

  if (foo (123) != 123)
  __builtin_abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "40 *\\*" 1 "switchconv" } } */
/* { dg-final { scan-tree-dump-not "switch" "optimized" } } */
