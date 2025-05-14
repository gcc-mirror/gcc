/* { dg-additional-options "-fdump-tree-gimple" }  */

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

void f1(omp_interop_t) { }
#pragma omp declare variant(f1) match(construct={dispatch}) \
  append_args(interop(target, prefer_type({attr("ompx_fun")})))
void g1(void);


int f2(omp_interop_t, omp_interop_t);
#pragma omp declare variant(f2) \
  append_args(interop(target, prefer_type("cuda")),			\
	      interop(prefer_type({fr("hsa")}),target))			\
  match(construct={dispatch})
int g2(void) { return 5; }

int foo (omp_interop_t obj1)
{
  omp_interop_t obj2 = omp_interop_none;
  int res;

  #pragma omp dispatch interop(obj1) device(11)
    g1();

  #pragma omp dispatch interop(obj1, obj2) device(22)
    g2();

  #pragma omp dispatch interop(obj2, obj1) device(33)
    res = g2();

  return res;
}

/* { dg-final { scan-tree-dump-times "D\.\[0-9\]+ = __builtin_omp_get_default_device \\(\\);" 3 "gimple" } }  */

/* { dg-final { scan-tree-dump-times "__builtin_omp_set_default_device \\(11\\);" 1 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "__builtin_omp_set_default_device \\(22\\);" 1 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "__builtin_omp_set_default_device \\(33\\);" 1 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "__builtin_omp_set_default_device \\(D\.\[0-9\]+\\);" 3 "gimple" } }  */

/* { dg-final { scan-tree-dump-times "  f1 \\(obj1\\);" 1 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "  f2 \\(obj1, obj2\\);" 1 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "  res = f2 \\(obj2, obj1\\);" 1 "gimple" } }  */
