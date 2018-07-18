// { dg-do compile }

// Origin: Stefan Straﬂer <sstrasser@systemhaus-gruppe.de>

// PR c++/20240: 

namespace A { int a; }

namespace C{
  int a;
  using A::a;		// { dg-error "conflicts with a previous" }
}
