/* PR tree-optimization/49352 */
/* { dg-do compile } */
/* { dg-additional-options "-O2 -fcompare-debug" } */
/* { dg-xfail-if "" { powerpc-ibm-aix* } { "*" } { "" } } */

int
foo (int *x, int *y, int n)
{
  int i, j;
  int dot = 0;
  for (i = 0; i < n; i++)
    for (j = 0; j < 2; j++)
      dot += *(x++) * *(y++);
  return dot;
}
