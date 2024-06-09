/* { dg-do compile } */
/* { dg-options "-O -ffinite-math-only -fno-signed-zeros -fstrict-overflow -fdump-tree-optimized" } */
/* { dg-additional-options "-msse -mfpmath=sse" { target { { i?86-*-* x86_64-*-* } && ilp32 } } } */

double g (double a)
{
  return (a<-a)?a:-a;
}
int f(int a)
{
  return (a<-a)?a:-a;
}

/* { dg-final { scan-tree-dump-times "\.COPYSIGN" 1 "optimized" { target ifn_copysign } } } */
/* { dg-final { scan-tree-dump-times "ABS_EXPR" 1 "optimized" { target ifn_copysign } } } */
/* { dg-final { scan-tree-dump-times "ABS_EXPR" 2 "optimized" { target { ! ifn_copysign } } } } */
