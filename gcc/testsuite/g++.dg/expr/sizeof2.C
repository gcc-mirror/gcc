// { dg-do compile }
// Contributed by Wolfgang Bangerth <bangerth at ticam dot utexas dot edu>
// PR c++/9259: Allow non-qualified member calls in sizeof expressions.

template <bool> struct StaticAssert;
template <> struct StaticAssert<true> {};

struct S 
{
  static int check ();
  static double check2 ();
  static const int value = sizeof(check());
  static const int value2 = sizeof(check2());
};

template <class>
struct T
{
  static int check ();
  static double check2 ();
  static const int value = sizeof(check());
  static const int value2 = sizeof(check2());
};

StaticAssert<(S::value == sizeof(int))> s;
StaticAssert<(S::value2 == sizeof(double))> s2;

StaticAssert<(T<void>::value == sizeof(int))> t;
StaticAssert<(T<void>::value2 == sizeof(double))> t2;

