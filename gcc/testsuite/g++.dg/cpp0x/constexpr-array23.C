// PR c++/86917
// { dg-do compile { target c++11 } }

struct A
{
  constexpr A () : c (0) {}
  static const A z;
  unsigned c;
};

struct B
{				// This should really be target { ! c++2a }
  typedef A W[4];		// { dg-error "paren" "" { target *-*-* } .+1 }
  constexpr B () : w ({ A::z, A::z, A::z, A::z }) {} // { dg-error "constant" }
  W w;
};

struct C
{
  C ();
  B w[1];
};

C::C () { }
