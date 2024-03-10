/* { dg-do compile } */
/* { dg-options "-O3 -march=znver4 --param vect-partial-vector-usage=2 -fno-vect-cost-model -fdump-tree-vect" } */

int a[4096];

void foo ()
{
  for (int i = 1; i < 4095; ++i)
    a[i] = 42;
}

/* { dg-final { scan-tree-dump-not "VIEW_CONVERT_EXPR" "vect" } } */
