/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-optimized" } */

extern void foo (int);

void
f (int *p, int n)
{
  foo (*(p + n++ * 4));
  foo (*(p + 32 + n++ * 4));
  foo (*(p + 16 + n * 4));
}

/* { dg-final { scan-tree-dump-times "\\+ 144|\\, 144>" 1 "optimized" { target { int32 } } } } */
/* { dg-final { scan-tree-dump-times "\\+ 72|\\, 72>" 1 "optimized" { target { int16 } } } } */
/* { dg-final { scan-tree-dump-times "\\+ 96|\\, 96>" 1 "optimized" { target { int32 } } } } */
/* { dg-final { scan-tree-dump-times "\\+ 48|\\, 48>" 1 "optimized" { target { int16 } } } } */
