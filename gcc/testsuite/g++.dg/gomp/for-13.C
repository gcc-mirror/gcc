// At one point in development, a typo disabled the remapping of the
// for iteration variable as private.

// { dg-do compile }
// { dg-options "-fopenmp -fdump-tree-lower" }

extern void bar(int);
void foo(void)
{
  int i;

#pragma omp parallel for default(none)
  for (i = 0; i < 10; i++)
    bar(i);
}

// { dg-final { scan-tree-dump-times "omp_data_o" 0 "lower" } }
// { dg-final { cleanup-tree-dump "lower" } }
