// { dg-options -std=c++11 }

struct A
{
  A();
};

struct B : A
{
  constexpr B(): A() { }	// { dg-error "A::A" }
};
