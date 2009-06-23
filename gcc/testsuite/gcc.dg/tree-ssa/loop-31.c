/* PR 32283 */

/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized" } */

short a[(2048)];
short foo (int len, int v)
{
  int i;
  for (i = 0; i < len; i++) {
      a[i] = v;
  }
  return a[0];
}

/* When we do not have addressing mode including multiplication,
   the memory access should be strength-reduced.  */
/* { dg-final { scan-tree-dump-times " \\+ 2" 1 "optimized" { target arm-*-* } } } */
/* { dg-final { scan-tree-dump-times " \\+ 2" 1 "optimized" { target { ia64-*-* && ilp32 } } } } */
/* { dg-final { scan-tree-dump-times " \\+ 2" 2 "optimized" { target { ia64-*-* && lp64 } } } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
