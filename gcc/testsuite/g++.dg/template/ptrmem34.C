// PR c++/93740
// { dg-do compile { target c++11 } }

struct A {
  void foo();
  void bar();
};

template<class T, T val>
struct const_val { };

template<int N, class T>
struct indexed_elem { };

using mem_fun_A_foo = const_val<decltype(&A::foo), &A::foo>;
using mem_fun_A_bar = const_val<decltype(&A::bar), &A::bar>;

struct A_indexed_member_funcs
  : indexed_elem<0, mem_fun_A_foo>,
    indexed_elem<1, mem_fun_A_bar>
{ };

template<class T, int N>
constexpr int index_of(indexed_elem<N, T>) { return N; }

static_assert(index_of<mem_fun_A_foo>(A_indexed_member_funcs{}) == 0, "");
static_assert(index_of<mem_fun_A_bar>(A_indexed_member_funcs{}) == 1, "");
