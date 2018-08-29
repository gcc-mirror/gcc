// PR c++/81054
// { dg-do compile { target c++11 } }

struct A
{
  volatile int i;
  constexpr A() : i() {}
};

struct B
{
  static constexpr A a {};  // { dg-error "not literal|in-class initialization" }
};
