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


template<typename T, typename T2, typename T3>
void g(T, T2, T3, T3);
#pragma omp declare variant(g) match(construct={dispatch}) append_args(interop(target),interop(targetsync))
template<typename T, typename T2>
void f(T x, T2 y);

void foo()
{
  omp_interop_t obj1;  /* { dg-note "'obj1' was declared here" }  */
  omp_interop_t obj2 = omp_interop_none;
  #pragma omp dispatch device(9) novariants(1)
     f<int, const char *>(2, (const char*)"abc");
  #pragma omp dispatch device(5) interop(obj1,obj2)
     f<int, const char *>(3, (const char*)"cde");  /* { dg-warning "'obj1' is used uninitialized \\\[-Wuninitialized\\\]" }  */

  #pragma omp dispatch device(9) novariants(1)
     f<int, omp_interop_t>(2, omp_interop_none);
  #pragma omp dispatch device(5) interop(obj1,obj2)
     f<int, omp_interop_t>(3, omp_interop_none);
}


template<typename Ta, typename Tb, typename Tc>
void varvar(Ta, Tb, Tc, Tc, ...);
#pragma omp declare variant(varvar) match(construct={dispatch}) append_args(interop(target),interop(targetsync))
template<typename Ta, typename Tb>
void varbase(Ta x, Tb y, ...);

void bar()
{
  omp_interop_t obj3 = omp_interop_none;
  omp_interop_t obj4;  /* { dg-note "'obj4' was declared here" } */
  #pragma omp dispatch device(3) nocontext(1)
     varbase<int, int>(10, 11, 101, 202, 303);
  #pragma omp dispatch device(7) interop(obj3,obj4)
     varbase<int, int>(20, 21, 111, 222, 333);  /* { dg-warning "'obj4' is used uninitialized \\\[-Wuninitialized\\\]" }  */

  #pragma omp dispatch device(3) nocontext(1)
     varbase<int, omp_interop_t>(10, omp_interop_none, 101, 202, 303);
  #pragma omp dispatch device(7) interop(obj3,obj4)
     varbase<int, omp_interop_t>(20, omp_interop_none, 111, 222, 333);
}



template<typename T>
void nargVar(T, T);
#pragma omp declare variant(nargVar) match(construct={dispatch}) append_args(interop(target),interop(targetsync))
void nargsBase();

void foobar()
{
  omp_interop_t obj5 = omp_interop_none;
  omp_interop_t obj6 = omp_interop_none;
  #pragma omp dispatch device(1) nocontext(1)
     nargsBase();
  #pragma omp dispatch device(2) interop(obj5,obj6)
     nargsBase();
}

/* { dg-final { scan-tree-dump-times "D\.\[0-9\]+ = __builtin_omp_get_default_device \\(\\);" 10 "gimple" } }  */

/* { dg-final { scan-tree-dump-times "__builtin_omp_set_default_device \\(9\\);" 2 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "__builtin_omp_set_default_device \\(5\\);" 2 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "__builtin_omp_set_default_device \\(3\\);" 2 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "__builtin_omp_set_default_device \\(7\\);" 2 "gimple" } }  */

/* { dg-final { scan-tree-dump-times "__builtin_omp_set_default_device \\(1\\);" 1 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "__builtin_omp_set_default_device \\(2\\);" 1 "gimple" } }  */

/* { dg-final { scan-tree-dump-times "__builtin_omp_set_default_device \\(D\.\[0-9\]+\\);" 10 "gimple" } }  */


/* { dg-final { scan-tree-dump-times "f<int, const char\\*> \\(2, \"abc\"\\);" 1 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "g<int, const char\\*, omp_interop_t> \\(3, \"cde\", obj1, obj2\\);" 1 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "f<int, omp_interop_t> \\(2, 0\\);" 1 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "g<int, omp_interop_t, omp_interop_t> \\(3, 0, obj1, obj2\\);" 1 "gimple" } }  */

/* { dg-final { scan-tree-dump-times "varbase<int, int> \\(10, 11, 101, 202, 303\\);" 1 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "varvar<int, int, omp_interop_t> \\(20, 21, obj3, obj4, 111, 222, 333\\);" 1 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "varbase<int, omp_interop_t> \\(10, 0, 101, 202, 303\\);" 1 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "varvar<int, omp_interop_t, omp_interop_t> \\(20, 0, obj3, obj4, 111, 222, 333\\);" 1 "gimple" } }  */

/* { dg-final { scan-tree-dump-times "nargsBase \\(\\);" 1 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "nargVar<omp_interop_t> \\(obj5, obj6\\);" 1 "gimple" } }  */
