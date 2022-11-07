// DR2507: Allow default arguments
// { dg-additional-options {-std=c++23} }

struct A
{
  void operator[](int, int = 42);
};
