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

void f1(omp_interop_t *) { }
/* { dg-error "argument 1 of 'f1' must be of 'omp_interop_t'" "" { target c } .-1 }  */
/* { dg-note "initializing argument 1 of 'void f1\\(omp_interop_t\\*\\)'" "" { target c++ } .-2 }  */
#pragma omp declare variant(f1) match(construct={dispatch}) \
  append_args(interop(targetsync, prefer_type({attr("ompx_fun")})))
void g1(void);
/* { dg-note "'append_args' specified here" "" { target c } .-2 }  */
/* { dg-error "cannot convert 'omp_interop_t' to 'omp_interop_t\\*'" "" { target c++ } .-4 }  */

int f2(omp_interop_t);
#pragma omp declare variant(f2)					\
  append_args(interop(targetsync, prefer_type("cuda")))		\
  match(construct={dispatch})
int g2(void) { return 5; }

int foo (omp_interop_t *obj1)
{
  int res;

  #pragma omp dispatch interop(obj1) device(11)  /* { dg-error "'obj1' must be of 'omp_interop_t'" }  */
    res = g2();
  return res;
}
