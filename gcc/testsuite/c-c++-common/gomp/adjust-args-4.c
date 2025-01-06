/* { dg-do compile } */
/* { dg-additional-options "-fdump-tree-gimple" } */

/* Ensure that adjust_args is only applied when variant substitution happens. */

void h(int *);
void f(int *);
#pragma omp declare variant(f) match(construct={dispatch}) adjust_args(need_device_ptr : x)
void g(int *x);

void foo(int *y)
{
  #pragma omp dispatch
    h(y);
  #pragma omp dispatch
    f(y);
  #pragma omp dispatch
    g(y);
}

/* { dg-final { scan-tree-dump-times "h \\(y\\);" 1 "gimple" } } */
/* { dg-final { scan-tree-dump-times "f \\(y\\);" 1 "gimple" } } */
/* { dg-final { scan-tree-dump-times "D\.\[0-9]+ = __builtin_omp_get_mapped_ptr \\(y, D\.\[0-9]+\\);" 1 "gimple" } } */
/* { dg-final { scan-tree-dump-times "f \\(D\.\[0-9]+\\);" 1 "gimple" } } */
