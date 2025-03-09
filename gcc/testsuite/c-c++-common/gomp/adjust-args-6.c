/* { dg-do compile } */
/* { dg-additional-options "-fdump-tree-gimple" } */

/* Ensure that adjust_args is only applied when variant substitution happens. */

extern int flag;

void h(int *);
void f(int *);
#pragma omp declare variant(f) match(construct={dispatch}, user={condition(flag)}) adjust_args(need_device_ptr : x)
void g(int *x);

void foo(int *y)
{
  #pragma omp dispatch
    h(y);	/* no substitution */
  #pragma omp dispatch
    f(y);	/* no substitution */
  #pragma omp dispatch
    g(y);	/* conditional with substitution for f */
}

/* { dg-final { scan-tree-dump-times "h \\(y\\);" 1 "gimple" } } */
/* { dg-final { scan-tree-dump-times "f \\(y\\);" 1 "gimple" } } */
/* { dg-final { scan-tree-dump-times "D\.\[0-9]+ = __builtin_omp_get_mapped_ptr \\(y, D\.\[0-9]+\\);" 1 "gimple" } } */
/* { dg-final { scan-tree-dump-times "f \\(D\.\[0-9]+\\);" 1 "gimple" } } */
/* { dg-final { scan-tree-dump-times "g \\(y\\);" 1 "gimple" } } */
