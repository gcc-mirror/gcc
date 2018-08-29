// PR c++/59930

namespace N {
  template < typename T > class A
  {
    // Injects N::N
    template < T > friend class N;
    // { dg-error "template parameter" "" { target *-*-* } .-1 }
    // { dg-error "redeclared"  "" { target *-*-* } .-2 }
  };
}

void f ()
{
  N::A < int > a1;
  N::A <short > a2;
}
