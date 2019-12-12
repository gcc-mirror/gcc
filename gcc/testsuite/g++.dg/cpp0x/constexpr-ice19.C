// PR c++/81054
// { dg-do compile { target c++11 } }

struct A
{
  volatile int i;
  constexpr A() : i() {}
};

struct B
{
  static constexpr A a {};  // { dg-error "22:the type .const A. of .constexpr. variable .B::a. is not literal" }
// { dg-error "in-class initialization" "" { target c++11 } .-1 }
};
