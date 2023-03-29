// { dg-do compile { target c++11 } }

struct A
{
  A();
};

struct B : A
{
  constexpr B(): A() { }	// { dg-error "A::A" "" { target c++20_down } }
};
