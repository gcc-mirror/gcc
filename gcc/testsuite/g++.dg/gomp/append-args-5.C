/* { dg-additional-options "-fdump-tree-gimple" }  */

/* Check that adjust_args applies to the right argument,
   if C++ inserts a 'this' pointer.  */

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


struct t1 {
  void f1(int *x, int *y, int *z, omp_interop_t);
  #pragma omp declare variant(f1) match(construct={dispatch}) \
                                  adjust_args(need_device_ptr : y) \
                                  append_args( interop(target))
  void g1(int *x, int *y, int *z);
};

struct t2 {
  void f2(int *x, int *y, int *z, omp_interop_t, ...);
  #pragma omp declare variant(f2) match(construct={dispatch}) \
                                  adjust_args(need_device_ptr : x, y, z) \
                                  append_args( interop(prefer_type("cuda","hip","hsa"),target, targetsync))
  void g2(int *x, int *y, int *z, ...);
};


omp_interop_t obj1, obj2;

void test(int *a1, int *b1, int *c1,
          int *a2, int *b2, int *c2,
          int *a3, int *b3, int *c3,
          int *x1, int *x2, int *x3,
          int *y1, int *y2, int *y3)
{
  struct t1 s1;
  struct t2 s2;

  #pragma omp dispatch interop(obj1)
   s1.g1 (a1, b1, c1);

  #pragma omp dispatch interop(obj2) device(5)
   s2.g2 (a2, b2, c2, y1, y2, y3);
}


/* { dg-final { scan-tree-dump-times "D\.\[0-9\]+ = __builtin_omp_get_default_device \\(\\);" 2 "gimple" } }  */

/* { dg-final { scan-tree-dump-times "__builtin_omp_set_default_device \\(D\.\[0-9\]+\\);" 3 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "__builtin_omp_set_default_device \\(5\\);" 1 "gimple" } }  */

/* { dg-final { scan-tree-dump-times "obj1.\[0-9\] = obj1;" 2 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "obj2.\[0-9\] = obj2;" 1 "gimple" } }  */

/* { dg-final { scan-tree-dump-times "D\.\[0-9\]+ = __builtin_omp_get_interop_int \\(obj1.\[0-9\], -5, 0B\\);" 1 "gimple" } }  */

/* { dg-final { scan-tree-dump-times "__builtin_omp_get_mapped_ptr" 4 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "D\.\[0-9\]+ = __builtin_omp_get_mapped_ptr \\(b1, D\.\[0-9\]+\\);" 1 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "D\.\[0-9\]+ = __builtin_omp_get_mapped_ptr \\(c2, 5\\);" 1 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "D\.\[0-9\]+ = __builtin_omp_get_mapped_ptr \\(b2, 5\\);" 1 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "D\.\[0-9\]+ = __builtin_omp_get_mapped_ptr \\(a2, 5\\);" 1 "gimple" } }  */

/* { dg-final { scan-tree-dump-times "t1::f1 \\(&s1, a1, D\.\[0-9\]+, c1, obj1.\[0-9\]\\);" 1 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "t2::f2 \\(&s2, D\.\[0-9\]+, D\.\[0-9\]+, D\.\[0-9\]+, obj2.\[0-9\], y1, y2, y3\\);" 1 "gimple" } }  */
