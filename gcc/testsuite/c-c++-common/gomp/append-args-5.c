/* { dg-additional-options "-fdump-tree-gimple" } */

/* Check that append_args is not applied when the outermost function
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
#pragma omp declare variant(v1) match(construct={dispatch},user={condition(1)}) append_args(interop(targetsync))
int b1(int);

int v2(int);
int v2a(int);
#pragma omp declare variant(v2) match(construct={dispatch},user={condition(1)})
#pragma omp declare variant(v2a) match(user={condition(1)})
int b2(int);


int test (int y1, int y2, int y3, int y4, int num1, int num2, int num3, int num4)
{
  int x1, x2, x3, x4; 
  omp_interop_t obj = omp_interop_none;

  #pragma omp dispatch device(num1) interop(obj)
    x1 = v1 (b2 (y1), omp_interop_none);

  /* No variant substitution because of nocontext */
  #pragma omp dispatch device(num2) nocontext(1) interop(obj)
    x2 = b1 (b2 (y2));

  /* No variant substitution because of novariants */
  #pragma omp dispatch device(num2) novariants(1) interop(obj)
    x3 = b1 (b2 (y3));

  /* OK */
  #pragma omp dispatch device(num4) nocontext(0) interop(obj)
    x4 = b1 (b2 (y4));

  return x1 + x2 + x3 + x4;
}
