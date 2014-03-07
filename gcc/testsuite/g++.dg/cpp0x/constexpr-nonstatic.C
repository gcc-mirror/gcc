// { dg-do compile { target c++11 } }

struct A
{
  constexpr int i;		// { dg-error "" }
};
