/* { dg-additional-options "-fdump-tree-gimple" } */

/* Check that append_args is not applied if there the outermost function
   in '#pragma omp dispatch' is not variant substituted.  */

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

int v1(int, omp_interop_t);
#pragma omp declare variant(v1) match(construct={dispatch}) append_args(interop(target))
int b1(int);

int v2(int);
int v2a(int);
#pragma omp declare variant(v2) match(construct={dispatch},user={condition(1)})
#pragma omp declare variant(v2a) match(user={condition(1)})
int b2(int);


int test (int x1, int x2, int y1, int y2, int z1, int z2, int num1, int num2, int num3)
{

/* { dg-final { scan-tree-dump-times "D\.\[0-9\]+ = __builtin_omp_get_default_device \\(\\);" 3 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "__builtin_omp_set_default_device \\(D\.\[0-9\]+\\);" 3 "gimple" } }  */

  #pragma omp dispatch
    x1 = v1 (b2 (x1), omp_interop_none);
/* { dg-final { scan-tree-dump-times "D\.\[0-9\]+ = v2 \\(x1\\);" 1 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "x1 = v1 \\(D\.\[0-9\]+, 0\\);" 1 "gimple" } }  */

  #pragma omp dispatch device(num1)
    x2 = v1 (b2 (x2), omp_interop_none);
/* { dg-final { scan-tree-dump-times "__builtin_omp_set_default_device \\(num1\\);" 1 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "D\.\[0-9\]+ = v2 \\(x2\\);" 1 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "x2 = v1 \\(D\.\[0-9\]+, 0\\);" 1 "gimple" } }  */


  #pragma omp dispatch nocontext(1)
    y1 = v1 (b2 (y1), omp_interop_none);
/* { dg-final { scan-tree-dump-times "D\.\[0-9\]+ = v2a \\(y1\\);" 1 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "y1 = v1 \\(D\.\[0-9\]+, 0\\);" 1 "gimple" } }  */

  #pragma omp dispatch nocontext(1) device(num2)
    y2 = v1 (b2 (y2), omp_interop_none);
/* { dg-final { scan-tree-dump-times "__builtin_omp_set_default_device \\(num2\\);" 1 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "D\.\[0-9\]+ = v2a \\(y2\\);" 1 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "y2 = v1 \\(D\.\[0-9\]+, 0\\);" 1 "gimple" } }  */


  #pragma omp dispatch novariants(1)
    z1 = v1 (b2 (z1), omp_interop_none);
/* { dg-final { scan-tree-dump-times "D\.\[0-9\]+ = v2 \\(z1\\);" 1 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "z1 = v1 \\(D\.\[0-9\]+, 0\\);" 1 "gimple" } }  */

  #pragma omp dispatch novariants(1) device(num3)
    z2 = v1 (b2 (z2), omp_interop_none);
/* { dg-final { scan-tree-dump-times "__builtin_omp_set_default_device \\(num3\\);" 1 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "D\.\[0-9\]+ = v2 \\(z2\\);" 1 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "z2 = v1 \\(D\.\[0-9\]+, 0\\);" 1 "gimple" } }  */

  return x1 + x2 + y1 + y2 + z1 + z2;
}
