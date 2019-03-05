/* PR tree-optimization/84436 */
/* { dg-options "-O2 -fdump-tree-switchconv -fdump-tree-optimized" } */
/* { dg-do run } */

int
__attribute__ ((noipa))
foo (int how)
{
  switch (how) {
    case 2: how = 205; break; /* how = 100 * index + 5 */
    case 3: how = 305; break;
    case 4: how = 405; break;
    case 5: how = 505; break;
    case 6: how = 605; break;
  }
  return how;
}

int main()
{
  if (foo (2) != 205)
  __builtin_abort ();

  if (foo (6) != 605)
  __builtin_abort ();

  if (foo (123) != 123)
  __builtin_abort ();

  return 0;
}


/* { dg-final { scan-tree-dump-times "100 \\*" 1 "switchconv" } } */
/* { dg-final { scan-tree-dump-times ".* \\+ 5" 1 "switchconv" } } */
/* { dg-final { scan-tree-dump-not "switch" "optimized" } } */
