/* { dg-do compile } */
/* { dg-additional-options "-fdump-tree-gimple" } */

/* Test that interop objects are implicitly created/destroyed when a dispatch
   construct doesn't provide enough of them to satisfy the declare variant
   append_args clause.  */

/* The following definitions are in omp_lib, which cannot be included
   in gcc/testsuite/  */

#if __cplusplus >= 201103L
# define __GOMP_UINTPTR_T_ENUM : __UINTPTR_TYPE__
#else
# define __GOMP_UINTPTR_T_ENUM
#endif

typedef enum omp_interop_t __GOMP_UINTPTR_T_ENUM
{
  omp_interop_none = 0,
  __omp_interop_t_max__ = __UINTPTR_MAX__
} omp_interop_t;

float repl1(omp_interop_t, omp_interop_t, omp_interop_t);

#pragma omp declare variant(repl1) match(construct={dispatch}) append_args(interop(target), interop(targetsync), interop (target))
float base1(void);

float
test (int *a, int *b)
{
  omp_interop_t obj1;
  float x;

  /* repl1 takes 3 interop arguments, one will come from the dispatch
     construct and the other 2 will be consed up.  */
  #pragma omp dispatch interop ( obj1 )
    x = base1 ();

  return x;
}

/* { dg-final { scan-tree-dump "__builtin_GOMP_interop \\(D\.\[0-9\]+, 2, &interopobjs\.\[0-9\]+, &tgt_tgtsync\.\[0-9\]+," "gimple" } } */
/* { dg-final { scan-tree-dump "__builtin_GOMP_interop \\(D\.\[0-9\]+, 0, 0B, 0B, 0B, 0, 0B, 2, &interopobjs\.\[0-9\]+," "gimple" } } */
/* { dg-final { scan-tree-dump "repl1 \\(obj1, interop\.\[0-9\]+, interop\.\[0-9\]+\\)" "gimple" } } */
