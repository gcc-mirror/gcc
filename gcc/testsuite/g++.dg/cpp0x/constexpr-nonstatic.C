// { dg-options -std=c++0x }

struct A
{
  constexpr int i;		// { dg-error "" }
};
