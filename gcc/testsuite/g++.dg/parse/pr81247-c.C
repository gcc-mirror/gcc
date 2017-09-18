// PR c++/81247 confused error

namespace N { // { dg-message "previous declaration" }
  template < typename T > class A
  { // { dg-error "conflicts with a previous" }
    template < T > friend class N;
  };
}

void f ()
{
  N::A < int > a1;
}
