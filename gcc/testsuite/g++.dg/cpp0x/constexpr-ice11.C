// PR c++/58610
// { dg-do compile { target c++11 } }

struct A
{
  template<typename> A();
};

constexpr A a;  // { dg-error "literal|matching" }
