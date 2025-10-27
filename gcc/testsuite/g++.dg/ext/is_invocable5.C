// PR c++/121055
// { dg-do compile { target c++11 } }
// { dg-skip-if "requires hosted libstdc++ for functional function" { ! hostedlib } }

#include <functional>

#define SA(X) static_assert((X),#X)

struct F;

SA( __is_invocable(void (F::*)() &, std::reference_wrapper<F>) );
SA( ! __is_invocable(void (F::*)() &&, std::reference_wrapper<F>) );

SA( __is_invocable(void (F::*)(int) &, std::reference_wrapper<F>, int) );
SA( ! __is_invocable(void (F::*)(int) &&, std::reference_wrapper<F>, int) );
