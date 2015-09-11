/* { dg-do compile } */
/* { dg-options "-O1 -fstrict-overflow -fdump-tree-optimized" } */

int f(int a)
{
  return -(a/10);
}

/* { dg-final { scan-tree-dump-times "a_\[0-9()D\]* / 10" 0 "optimized"} } */
/* { dg-final { scan-tree-dump-times "a_..D. / -10" 1 "optimized"} } */



