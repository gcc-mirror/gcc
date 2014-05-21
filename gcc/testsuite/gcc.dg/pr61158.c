/* PR tree-optimization/61158 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-original" } */

unsigned long long
foo (unsigned int x)
{
  return ((unsigned long long) x & 0x00ff000000000000ULL) >> 40;
}

/* { dg-final { scan-tree-dump "return 0;" "original" { target { ilp32 || lp64 } } } } */
/* { dg-final { cleanup-tree-dump "original" } } */
