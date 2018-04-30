// PR c++/81247 confused error

namespace N { // { dg-message "previous declaration" }
}

template < typename T > class A
{ // { dg-error "redeclared as different" }
  template < T > friend class N;
};

void f ()
{
  A < int > a1;
}
