/* { dg-do compile } */

/* At least one of the target and/or targetsync modifiers must be provided.  */

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
  append_args(interop(prefer_type({attr("ompx_fun")})))
// { dg-error "missing required 'target' and/or 'targetsync' modifier" ""  { target *-*-* } .-1 }
void g1(void);


int f2(omp_interop_t, omp_interop_t);
#pragma omp declare variant(f2) \
  append_args(interop(prefer_type("cuda")),			\
	      interop(prefer_type({fr("hsa")})))			\
  match(construct={dispatch})
// { dg-error "missing required 'target' and/or 'targetsync' modifier" ""  { target *-*-* } .-3 }
// { dg-error "missing required 'target' and/or 'targetsync' modifier" ""  { target *-*-* } .-3 }
int g2(void) { return 5; }
