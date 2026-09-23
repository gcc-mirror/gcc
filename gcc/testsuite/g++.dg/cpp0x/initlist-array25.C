// PR c++/126752
// { dg-do compile { target c++11 } }

struct A {
  struct B { constexpr B () {} };
  struct C { ~C (); B c; };
  struct D : private C {};
  struct E { E (); };
  struct F { D f {}; };
  struct G { F g[1] {}; E h; };
  void foo () { G {}; }
};
