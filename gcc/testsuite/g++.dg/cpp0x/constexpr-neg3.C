// PR c++/58612
// { dg-do compile { target c++11 } }

struct A
{
  int foo() const { return 0; }
};

template<typename> struct B
{
  A a;
  constexpr int bar() { return a.foo(); } // { dg-error "foo" "" { target { ! implicit_constexpr } } }
};

constexpr int i = B<void>().bar(); // { dg-error "bar" "" { target { ! implicit_constexpr } } }
