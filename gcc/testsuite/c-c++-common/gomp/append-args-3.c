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

void g1(int, const char *, omp_interop_t);
#pragma omp declare variant(g1) match(construct={dispatch}) append_args(interop(target,targetsync))
void f1(int x, const char *y);

void g2(int, const char *, omp_interop_t);
#pragma omp declare variant(g2) match(construct={dispatch}) append_args(interop(target,targetsync)) adjust_args(need_device_ptr: y)
void f2(int x, const char *y);

void foo()
{
  omp_interop_t obj1;  /* { dg-note "'obj1' was declared here" }  */
  omp_interop_t obj2 = omp_interop_none;
  omp_interop_t obj5 = omp_interop_none;
  omp_interop_t obj6 = omp_interop_none;
  const char *cp = 0L;

  #pragma omp dispatch device(9) novariants(1)
     f(2, "abc");
  #pragma omp dispatch device(5) interop(obj1,obj2)
     f(3, "cde");  /* { dg-warning "'obj1' is used uninitialized \\\[-Wuninitialized\\\]" }  */
  #pragma omp dispatch interop(obj5)
     f1(4, "fgh");
  #pragma omp dispatch interop(obj6)
     f2(5, cp);
}


void varvar(int, int, omp_interop_t, omp_interop_t, ...);
#pragma omp declare variant(varvar) match(construct={dispatch}) append_args(interop(target),interop(targetsync))
void varbase(int x, int y, ...);

void varvar1(int, int, omp_interop_t, ...);
#pragma omp declare variant(varvar1) match(construct={dispatch}) append_args(interop(target,targetsync))
void varbase1(int x, int y, ...);

void varvar2(int, int *, omp_interop_t, ...) { }
#pragma omp declare variant(varvar2) match(construct={dispatch}) append_args(interop(target,targetsync)) adjust_args(need_device_ptr: y)
void varbase2(int x, int *y, ...) { }


void bar()
{
  omp_interop_t obj3 = omp_interop_none;
  omp_interop_t obj4;  /* { dg-note "'obj4' was declared here" } */
  omp_interop_t obj7 = omp_interop_none;
  omp_interop_t obj8 = omp_interop_none;
  int *itr = 0L;

  #pragma omp dispatch device(3) nocontext(1)
     varbase(10, 11, 101, 202, 303);
  #pragma omp dispatch device(7) interop(obj3,obj4)
     varbase(20, 21, 111, 222, 333);  /* { dg-warning "'obj4' is used uninitialized \\\[-Wuninitialized\\\]" }  */
  #pragma omp dispatch interop(obj7)
     varbase1(40, 31, 911, 922, 933);
  #pragma omp dispatch interop(obj8)
     varbase2(49, itr, 919, 929, 939);
}

/* { dg-final { scan-tree-dump-times "D\.\[0-9\]+ = __builtin_omp_get_default_device \\(\\);" 8 "gimple" } }  */

/* { dg-final { scan-tree-dump-times "__builtin_omp_set_default_device \\(9\\);" 1 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "__builtin_omp_set_default_device \\(5\\);" 1 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "__builtin_omp_set_default_device \\(3\\);" 1 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "__builtin_omp_set_default_device \\(7\\);" 1 "gimple" } }  */

/* { dg-final { scan-tree-dump-times "D\.\[0-9\]+ = __builtin_omp_get_interop_int \\(obj5, -5, 0B\\);" 1 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "D\.\[0-9\]+ = __builtin_omp_get_interop_int \\(obj6, -5, 0B\\);" 1 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "D\.\[0-9\]+ = __builtin_omp_get_interop_int \\(obj7, -5, 0B\\);" 1 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "D\.\[0-9\]+ = __builtin_omp_get_interop_int \\(obj8, -5, 0B\\);" 1 "gimple" } }  */

/* { dg-final { scan-tree-dump-times "__builtin_omp_set_default_device \\(D\.\[0-9\]+\\);" 12 "gimple" } }  */

/* { dg-final { scan-tree-dump-times "f \\(2, \"abc\"\\);" 1 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "g \\(3, \"cde\", obj1, obj2\\);" 1 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "g1 \\(4, \"fgh\", obj5\\);" 1 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "D\.\[0-9\]+ = __builtin_omp_get_mapped_ptr \\(cp, D\.\[0-9\]+\\);" 1 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "g2 \\(5, D\.\[0-9\]+, obj6\\);" 1 "gimple" } }  */

/* { dg-final { scan-tree-dump-times "varbase \\(10, 11, 101, 202, 303\\);" 1 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "varvar \\(20, 21, obj3, obj4, 111, 222, 333\\);" 1 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "varvar1 \\(40, 31, obj7, 911, 922, 933\\);" 1 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "D\.\[0-9\]+ = __builtin_omp_get_mapped_ptr \\(itr, D\.\[0-9\]+\\);" 1 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "varvar2 \\(49, D\.\[0-9\]+, obj8, 919, 929, 939\\);" 1 "gimple" } }  */
