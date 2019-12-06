// PR c++/61414
// { dg-do compile { target c++11 } }

enum C { C0 = -4, C1 = 3 };
enum D { D0 = 0, D1 = 15 };
enum class E { E0 = -4, E1 = 3 };
enum F : unsigned { F0 = 0, F1 = 15 };
enum __attribute__((__mode__ (__QI__))) G { G0 = -4, G1 = 3 };
enum __attribute__((__mode__ (__HI__))) H { H0 = 0, H1 = 15 };

struct S
{
  C a : 2;	// { dg-warning "'S::a' is too small to hold all values of 'enum C'" }
  C b : 3;	// { dg-bogus "'S::b' is too small to hold all values of 'enum C'" }
  D c : 3;	// { dg-warning "'S::c' is too small to hold all values of 'enum D'" }
  D d : 4;	// { dg-bogus "'S::d' is too small to hold all values of 'enum D'" }
  E e : 2;	// { dg-warning "'S::e' is too small to hold all values of 'enum class E'" }
  E f : 3;	// { dg-bogus "'S::f' is too small to hold all values of 'enum class E'" }
  F g : 3;	// { dg-warning "'S::g' is too small to hold all values of 'enum F'" }
  F h : 4;	// { dg-bogus "'S::h' is too small to hold all values of 'enum F'" }
  G i : 2;	// { dg-warning "'S::i' is too small to hold all values of 'enum G'" }
  G j : 3;	// { dg-bogus "'S::j' is too small to hold all values of 'enum G'" }
  H k : 3;	// { dg-warning "'S::k' is too small to hold all values of 'enum H'" }
  H l : 4;	// { dg-bogus "'S::l' is too small to hold all values of 'enum H'" }
};
