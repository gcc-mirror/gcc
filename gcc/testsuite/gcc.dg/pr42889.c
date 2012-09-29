/* PR rtl-optimization/42889 */
/* { dg-do compile } */
/* { dg-options "-O -fgcse -fcompare-debug" } */
/* { dg-xfail-if "" { powerpc-ibm-aix* } { "*" } { "" } } */

extern int A[], B[];

int
foo (int x, int c, int i)
{
  if (A[i] && B[i])
    x = x % ((c & 4) ? 8 : 4);
  return x;
}
