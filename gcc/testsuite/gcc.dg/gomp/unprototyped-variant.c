/* { dg-additional-options "-std=gnu90" } */

/* This test case used to ICE in the gimplifier after issuing a 
   different diagnostic message.  */

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


void g2();
#pragma omp declare variant(g2) match(construct={dispatch}) append_args(interop(target,targetsync))  /* { dg-message "'append_args' with unprototyped base function" } */
void f2();

void foo()
{
  omp_interop_t obj6 = omp_interop_none;
  const char *cp = 0L;

  #pragma omp dispatch interop(obj6)
     f2(5, cp);
}
