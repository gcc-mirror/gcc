/* { dg-additional-options "-fdump-tree-gimple -Wall" }  */

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


void g(int, const char *, omp_interop_t, omp_interop_t);
#pragma omp declare variant(g) match(construct={dispatch}) append_args(interop(target),interop(targetsync))
void f(int x, const char *y);

void foo()
{
  omp_interop_t obj1;  /* { dg-note "'obj1' was declared here" }  */
  omp_interop_t obj2 = omp_interop_none;
  #pragma omp dispatch device(9) novariants(1)
     f(2, "abc");
  #pragma omp dispatch device(5) interop(obj1,obj2)
     f(3, "cde");  /* { dg-warning "'obj1' is used uninitialized \\\[-Wuninitialized\\\]" }  */
}


void varvar(int, int, omp_interop_t, omp_interop_t, ...);
#pragma omp declare variant(varvar) match(construct={dispatch}) append_args(interop(target),interop(targetsync))
void varbase(int x, int y, ...);

void bar()
{
  omp_interop_t obj3 = omp_interop_none;
  omp_interop_t obj4;  /* { dg-note "'obj4' was declared here" } */
  #pragma omp dispatch device(3) nocontext(1)
     varbase(10, 11, 101, 202, 303);
  #pragma omp dispatch device(7) interop(obj3,obj4)
     varbase(20, 21, 111, 222, 333);  /* { dg-warning "'obj4' is used uninitialized \\\[-Wuninitialized\\\]" }  */
}

/* { dg-final { scan-tree-dump-times "D\.\[0-9\]+ = __builtin_omp_get_default_device \\(\\);" 4 "gimple" } }  */

/* { dg-final { scan-tree-dump-times "__builtin_omp_set_default_device \\(9\\);" 1 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "__builtin_omp_set_default_device \\(5\\);" 1 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "__builtin_omp_set_default_device \\(3\\);" 1 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "__builtin_omp_set_default_device \\(7\\);" 1 "gimple" } }  */

/* { dg-final { scan-tree-dump-times "__builtin_omp_set_default_device \\(D\.\[0-9\]+\\);" 4 "gimple" } }  */

/* { dg-final { scan-tree-dump-times "f \\(2, \"abc\"\\);" 1 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "g \\(3, \"cde\", obj1, obj2\\);" 1 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "varbase \\(10, 11, 101, 202, 303\\);" 1 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "varvar \\(20, 21, obj3, obj4, 111, 222, 333\\);" 1 "gimple" } }  */
