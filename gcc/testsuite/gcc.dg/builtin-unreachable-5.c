/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-fab" } */

int
foo (int a)
{
  if (a <= 0)
    {
    L1:
      __builtin_unreachable ();
    }

  if (a > 2)
    goto L1;

  return a > 0;
}

/* { dg-final { scan-tree-dump-times "if \\(" 0 "fab" } } */
/* { dg-final { scan-tree-dump-times "goto" 0 "fab" } } */
/* { dg-final { scan-tree-dump-times "L1:" 0 "fab" } } */
/* { dg-final { scan-tree-dump-times "__builtin_unreachable" 0 "fab" } } */
/* { dg-final { cleanup-tree-dump "fab" } } */
