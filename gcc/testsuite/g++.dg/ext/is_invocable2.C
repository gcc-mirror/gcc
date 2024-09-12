// { dg-do compile { target c++11 } }
// { dg-skip-if "requires hosted libstdc++ for functional function" { ! hostedlib } }
// __is_invocable should handle std::reference_wrapper correctly.

#include <functional>

#define SA(X) static_assert((X),#X)

using std::reference_wrapper;

using func_type_v0 = void(*)();

SA(   __is_invocable( reference_wrapper<func_type_v0> ) );
SA( ! __is_invocable( reference_wrapper<func_type_v0>, int ) );

using func_type_i0 = int(*)();

SA(   __is_invocable( reference_wrapper<func_type_i0> ) );
SA( ! __is_invocable( reference_wrapper<func_type_i0>, int ) );

using func_type_l0 = int&(*)();

SA(   __is_invocable( reference_wrapper<func_type_l0> ) );
SA( ! __is_invocable( reference_wrapper<func_type_l0(int)> ) );

using func_type_ii = int(*)(int);

SA( ! __is_invocable( reference_wrapper<func_type_ii> ) );
SA(   __is_invocable( reference_wrapper<func_type_ii>, int ) );

using func_type_il = int(*)(int&);

SA( ! __is_invocable( reference_wrapper<func_type_il> ) );
SA( ! __is_invocable( reference_wrapper<func_type_il>, int ) );
SA(   __is_invocable( reference_wrapper<func_type_il>, int& ) );

using func_type_ir = int(*)(int&&);

SA( ! __is_invocable( reference_wrapper<func_type_ir> ) );
SA( ! __is_invocable( reference_wrapper<func_type_ir>, int& ) );
SA(   __is_invocable( reference_wrapper<func_type_ir>, int ) );
SA(   __is_invocable( reference_wrapper<func_type_ir>, int&& ) );

struct A { };

using mem_type_i = int A::*;

SA( ! __is_invocable( reference_wrapper<mem_type_i> ) );
SA( ! __is_invocable( reference_wrapper<mem_type_i>, int ) );
SA( ! __is_invocable( reference_wrapper<mem_type_i>, int* ) );
SA( ! __is_invocable( reference_wrapper<mem_type_i>, int& ) );
SA( ! __is_invocable( reference_wrapper<mem_type_i>, int&& ) );
SA(   __is_invocable( reference_wrapper<mem_type_i>, A ) );
SA(   __is_invocable( reference_wrapper<mem_type_i>, A* ) );
SA(   __is_invocable( reference_wrapper<mem_type_i>, A& ) );
SA(   __is_invocable( reference_wrapper<mem_type_i>, A&& ) );

using memfun_type_i = int (A::*)();

SA( ! __is_invocable( reference_wrapper<memfun_type_i> ) );
SA( ! __is_invocable( reference_wrapper<memfun_type_i>, int ) );
SA( ! __is_invocable( reference_wrapper<memfun_type_i>, int* ) );
SA( ! __is_invocable( reference_wrapper<memfun_type_i>, int& ) );
SA( ! __is_invocable( reference_wrapper<memfun_type_i>, int&& ) );
SA(   __is_invocable( reference_wrapper<memfun_type_i>, A ) );
SA(   __is_invocable( reference_wrapper<memfun_type_i>, A* ) );
SA(   __is_invocable( reference_wrapper<memfun_type_i>, A& ) );
SA(   __is_invocable( reference_wrapper<memfun_type_i>, A&& ) );
SA( ! __is_invocable( reference_wrapper<memfun_type_i>, const A& ) );
SA( ! __is_invocable( reference_wrapper<memfun_type_i>, A&, int ) );

using memfun_type_ic = int (A::*)() const;

SA( ! __is_invocable( reference_wrapper<memfun_type_ic> ) );
SA( ! __is_invocable( reference_wrapper<memfun_type_ic>, int ) );
SA( ! __is_invocable( reference_wrapper<memfun_type_ic>, int& ) );
SA(   __is_invocable( reference_wrapper<memfun_type_ic>, A& ) );
SA(   __is_invocable( reference_wrapper<memfun_type_ic>, A* ) );
SA( ! __is_invocable( reference_wrapper<memfun_type_ic>, A&, int ) );
SA( ! __is_invocable( reference_wrapper<memfun_type_ic>, A*, int& ) );
SA(   __is_invocable( reference_wrapper<memfun_type_ic>, const A& ) );
SA(   __is_invocable( reference_wrapper<memfun_type_ic>, const A* ) );
SA( ! __is_invocable( reference_wrapper<memfun_type_ic>, const A&, int& ) );
SA( ! __is_invocable( reference_wrapper<memfun_type_ic>, const A*, int ) );

using memfun_type_iic = int& (A::*)(int&) const;

SA( ! __is_invocable( reference_wrapper<memfun_type_iic> ) );
SA( ! __is_invocable( reference_wrapper<memfun_type_iic>, int ) );
SA( ! __is_invocable( reference_wrapper<memfun_type_iic>, int& ) );
SA( ! __is_invocable( reference_wrapper<memfun_type_iic>, A&, int ) );
SA(   __is_invocable( reference_wrapper<memfun_type_iic>, A&, int& ) );
SA( ! __is_invocable( reference_wrapper<memfun_type_iic>, A*, int ) );
SA(   __is_invocable( reference_wrapper<memfun_type_iic>, A*, int& ) );
SA( ! __is_invocable( reference_wrapper<memfun_type_iic>, const A&, int ) );
SA( ! __is_invocable( reference_wrapper<memfun_type_iic>, const A&, int&, int ) );
SA(   __is_invocable( reference_wrapper<memfun_type_iic>, const A&, int& ) );
SA(   __is_invocable( reference_wrapper<memfun_type_iic>, const A*, int& ) );

struct B {
  int& operator()();
  long& operator()() const;
  bool& operator()(int);
private:
  void operator()(int, int);
};
using CB = const B;

SA(   __is_invocable( reference_wrapper<B> ) );
SA(   __is_invocable( reference_wrapper<B>& ) );
SA(   __is_invocable( reference_wrapper<B>&& ) );
SA(   __is_invocable( reference_wrapper<CB> ) );
SA(   __is_invocable( reference_wrapper<CB>& ) );
SA(   __is_invocable( reference_wrapper<B>, int ) );
SA( ! __is_invocable( reference_wrapper<B>&, int, int ) );

struct C : B { int& operator()() = delete; };
using CC = const C;

SA( ! __is_invocable( reference_wrapper<C> ) );
SA( ! __is_invocable( reference_wrapper<C>& ) );
SA( ! __is_invocable( reference_wrapper<C>&& ) );
SA( ! __is_invocable( reference_wrapper<CC> ) );
SA( ! __is_invocable( reference_wrapper<CC>& ) );

struct D { B operator*(); };
using CD = const D;

SA( ! __is_invocable( reference_wrapper<D> ) );
SA( ! __is_invocable( reference_wrapper<D>& ) );
SA( ! __is_invocable( reference_wrapper<D>&& ) );
SA( ! __is_invocable( reference_wrapper<D>* ) );
SA( ! __is_invocable( reference_wrapper<D*> ) );
SA( ! __is_invocable( reference_wrapper<D*>* ) );

std::function<void()> fn = []() {};
auto refwrap = std::ref(fn);

SA(   __is_invocable( decltype(fn) ) );
SA(   __is_invocable( decltype(refwrap) ) );
