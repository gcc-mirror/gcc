/* PR debug/46799 */
/* { dg-do compile } */
/* { dg-options "-O -ftree-parallelize-loops=2 -fno-tree-dce -ftree-pre -fcompare-debug" } */
/* { dg-xfail-if "compare-debug failure" { powerpc-ibm-aix* } { "*" } { "" } } */

int
foo (int i, int *a)
{
  int e;
  for (; i; i++)
    e = *a;
  return e;
}
