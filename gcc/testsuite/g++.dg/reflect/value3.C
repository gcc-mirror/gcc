// PR c++/125820
// P4101R1, Consteval-only Values for C++26
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection -g" }

using info = decltype(^^::);

struct A { info i; };
struct B : A { };
struct C { A a; };
struct D { info i1, i2, i3; };
struct E { C c; info i; };
struct F { A a[10]; };
struct G { info i = info{}; };

A a1 = { info{} };
A a2{ info{} };
B b1 = { info{} };
B b2{ info{} };
C c1 = { { info{} } };
C c2{ { info{} } };
D d1 = { info{}, info{}, info{} };
D d2{ info{}, info{}, info{} };
E e1 = { { { info{} } }, info{} };
E e2{ { { info{} } }, info{} };
F f1 = { { { info{} }, { info{} } } };
F f2{ { { info{} }, { info{} } } };
G g1{};
G g2;

void
fn ()
{
  A la1 = { info{} };
  A la2{ info{} };
  B lb1 = { info{} };
  B lb2{ info{} };
  C lc1 = { { info{} } };
  C lc2{ { info{} } };
  D ld1 = { info{}, info{}, info{} };
  D ld2{ info{}, info{}, info{} };
  E le1 = { { { info{} } }, info{} };
  E le2{ { { info{} } }, info{} };
  F lf1 = { { { info{} }, { info{} } } };
  F lf2{ { { info{} }, { info{} } } };
  G lg1{};
  G lg2;

  static A sa1 = { info{} };
  static A sa2{ info{} };
  static B sb1 = { info{} };
  static B sb2{ info{} };
  static C sc1 = { { info{} } };
  static C sc2{ { info{} } };
  static D sd1 = { info{}, info{}, info{} };
  static D sd2{ info{}, info{}, info{} };
  static E se1 = { { { info{} } }, info{} };
  static E se2{ { { info{} } }, info{} };
  static F sf1 = { { { info{} }, { info{} } } };
  static F sf2{ { { info{} }, { info{} } } };
  static G sg1{};
  static G sg2;
}
