/* PR c++/119601 */
/* { dg-do compile { target c++11 } } */
/* { dg-additional-options "-fdump-tree-gimple" } */

/* Parameter pack cases, check that interop arguments are passed correctly.  */

#include "append-args-omp-interop-t.h"

template<typename... Args>
void v0(int*, Args...) {}

#pragma omp declare variant(v0) match(construct={dispatch}) \
				append_args(interop(target))
template<typename... Args>
void b0(int *, Args...) {}


template<typename... Args>
void v1(int*, Args...) {}

#pragma omp declare variant(v1) match(construct={dispatch}) \
				append_args(interop(target), \
					    interop(targetsync))
template<typename... Args>
void b1(int *, Args...) {}


void f1(int *p0, int *p1)
{
  #pragma omp dispatch
  b0(p0);
/* { dg-final { scan-tree-dump "v0<\[^>\]*> \\(p0, interop\.\[0-9\]\\);" "gimple" } }  */
  #pragma omp dispatch
  b1(p1, 1, 2, 3);
/* { dg-final { scan-tree-dump "v1<int, int, int, omp_interop_t, omp_interop_t> \\(p1, 1, 2, 3, interop\.\[0-9\], interop\.\[0-9\]\\);" "gimple" } }  */
}