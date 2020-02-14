// PR c++/61414
// { dg-do compile { target c++11 } }

enum class E { E0 = -4, E1 = 3 };
enum F : unsigned { F0 = 0, F1 = 15 };

struct S
{
  E a : 2;	// { dg-warning "'S::a' is too small to hold all values of 'enum class E'" }
  E b : 2;	// { dg-warning "'S::b' is too small to hold all values of 'enum class E'" }
  E c : 3;	// { dg-bogus "'S::c' is too small to hold all values of 'enum class E'" }
  F d : 3;	// { dg-warning "'S::d' is too small to hold all values of 'enum F'" }
  F e : 3;	// { dg-warning "'S::e' is too small to hold all values of 'enum F'" }
  F f : 4;	// { dg-bogus "'S::f' is too small to hold all values of 'enum F'" }
};
