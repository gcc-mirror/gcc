/* { dg-do compile } */
/* { dg-options "-O1 -fno-tree-dce -fdump-tree-optimized" } */
_Complex int t = 0;
int f(void)
{
  t = 0;
 __real__ t = 2;
 __imag__ t = 2;
}

/* { dg-final { scan-tree-dump-times "__complex__" 0 "optimized" { xfail *-*-* } } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
