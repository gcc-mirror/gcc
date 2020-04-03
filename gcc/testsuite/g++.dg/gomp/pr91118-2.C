// PR c++/91118
// { dg-do compile }

#include <typeinfo>

struct S { virtual ~S (); };
void bar (const std::type_info &, const std::type_info &);

void
foo (S *p)
{
  #pragma omp parallel default (none) firstprivate (p)
    bar (typeid (*p), typeid (S));
}
