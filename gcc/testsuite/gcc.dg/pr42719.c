/* PR tree-optimization/42719 */
/* { dg-do compile } */
/* { dg-options "-O2 -ftracer -fcompare-debug" } */
/* { dg-xfail-if "" { powerpc-ibm-aix* } { "*" } { "" } } */

int *v;

void
foo (int a)
{
  int i, j;
  for (j = i = a; i != -1; j = i, i = v[i])
    ;
  v[j] = v[a];
}
