// { dg-options -std=c++11 }

struct A
{
  constexpr int i;		// { dg-error "" }
};
