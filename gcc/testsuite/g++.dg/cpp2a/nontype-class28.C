// PR c++/92948 - Fix class NTTP with template arguments.
// { dg-do compile { target c++20 } }

struct A {
  constexpr A(int) { }
};

template<A>
struct B {
  using U = unsigned;
};

template<A a>
using U = B<a>;

template<int X, typename Y = typename B<X>::U>
void foo()
{
}

template<int X, typename Y = typename U<X>::U>
void foo2()
{
}

template<typename Y = typename B<1>::U>
void foo3()
{
}

void
g ()
{
  foo<1>();
  foo2<1>();
  foo3<>();
}
