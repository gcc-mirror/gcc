/* PR c++/119775 */

/* { dg-additional-options "-fdump-tree-gimple" }  */

#include "append-args-omp-interop-t.h"

struct S {
  void v(int *, omp_interop_t) {}

  #pragma omp declare variant(v) match(construct={dispatch}) \
                                 append_args(interop(target))
  void b(int *) {}
};

void f(int *p)
{
  S s = S();
  #pragma omp dispatch
  s.b(p);
/* { dg-final { scan-tree-dump "S::v \\(&s, p,\ interop\.\[0-9\]+\\);" "gimple" } }  */
}
