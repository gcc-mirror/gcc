/* { dg-do compile } */
/* { dg-options "-O2 -funswitch-loops -fdump-tree-unswitch-optimized --param max-unswitch-insns=100" } */

void bar (int);
void foo (int a, int b, int c, int n)
{
  for (int i = 0; i < n; ++i)
    {
      if (a > 5)
        bar (1);
      if (b < 10)
        bar (2);
      if (c != 5)
        bar (3);
    }
}

/* Verify we can unswitch all permutations of the predicates.  */
/* { dg-final { scan-tree-dump-times "unswitching loop . on .if. with condition" 7 "unswitch" } } */
/* { dg-final { scan-tree-dump "unswitching loop . on .if. with condition: a" "unswitch" } } */
/* { dg-final { scan-tree-dump "unswitching loop . on .if. with condition: b" "unswitch" } } */
/* { dg-final { scan-tree-dump "unswitching loop . on .if. with condition: c" "unswitch" } } */
