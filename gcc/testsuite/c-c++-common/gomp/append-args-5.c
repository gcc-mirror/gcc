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
  /* { dg-note "'v1' declared here" "" { target c } .-1 } */
  /* { dg-note "'int v1\\(int, omp_interop_t\\)' declared here" "" { target c++ } .-2 } */
int v1a(int);
  /* { dg-note "'declare variant' candidate 'v1a' declared here" "" { target c } .-1 } */
  /* { dg-note "'declare variant' candidate 'int v1a\\(int\\)' declared here" "" { target c++ } .-2 } */
#pragma omp declare variant(v1) match(construct={dispatch},user={condition(1)}) append_args(interop(targetsync))
#pragma omp declare variant(v1a) match(user={condition(1)})
int b1(int);
  /* { dg-note "'b1' declared here" "" { target c } .-1 } */
  /* { dg-note "'int b1\\(int\\)' declared here" "" { target c++ } .-2 } */

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
      /* { dg-error "unexpected 'interop' clause as invoked procedure 'v1' is not variant substituted" "" { target c } .-2 }  */
      /* { dg-error "unexpected 'interop' clause as invoked procedure 'int v1\\(int, omp_interop_t\\)' is not variant substituted" "" { target c++ } .-3 }  */


  #pragma omp dispatch device(num2) nocontext(1) interop(obj)
    x2 = b1 (b2 (y2));
      /* { dg-error "number of list items in 'interop' clause \\(1\\) exceeds the number of 'append_args' items \\(0\\) for 'declare variant' candidate 'v1a'" "" { target c } .-2 }  */
      /* { dg-error "number of list items in 'interop' clause \\(1\\) exceeds the number of 'append_args' items \\(0\\) for 'declare variant' candidate 'int v1a\\(int\\)'" "" { target c++ } .-3 }  */


  #pragma omp dispatch device(num2) novariants(1) interop(obj)
    x3 = b1 (b2 (y3));
      /* { dg-error "unexpected 'interop' clause as invoked procedure 'b1' is not variant substituted" "" { target c } .-2 }  */
      /* { dg-error "unexpected 'interop' clause as invoked procedure 'int b1\\(int\\)' is not variant substituted" "" { target c++ } .-3 }  */


  /* OK */
  #pragma omp dispatch device(num4) nocontext(0) interop(obj)
    x4 = b1 (b2 (y4));

  return x1 + x2 + x3 + x4;
}
