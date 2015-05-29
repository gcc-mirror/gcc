/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

int
foo (int a, int b)
{
  return ((a && !b) || (!a && b));
}

/* { dg-final { scan-tree-dump-times "\\\^" 1 "optimized" { target i?86-*-* x86_64-*-* } } } */
