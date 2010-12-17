// { dg-options -std=c++0x }

struct A
{
  A();
};

struct B : A
{
  constexpr B(): A() { }	// { dg-error "A::A" }
};
