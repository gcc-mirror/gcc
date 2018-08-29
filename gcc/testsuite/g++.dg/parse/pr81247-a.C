// PR c++/81247 ICE

namespace N  // { dg-message "previous declaration" }
// { dg-error "expected" "" { target *-*-* } .+1 }
template < typename T > class A
{ // { dg-error "redeclared as different" }
  template < T > friend class N;
};

void f ()
{
  A < int > a1; //  { dg-message "required from here" }
}
