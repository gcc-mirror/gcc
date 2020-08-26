// PR c++/86917
// { dg-do compile { target c++11 } }

struct A
{
  constexpr A () : c (0) {}
  static const A z;
  unsigned c;
};

struct B
{
  typedef A W[4];		// { dg-error "paren" "" { target { ! c++20 } } .+1 }
  constexpr B () : w ({ A::z, A::z, A::z, A::z }) {} // { dg-error "constant|could not convert" }
  W w;
};

struct C
{
  C ();
  B w[1];
};

C::C () { }
