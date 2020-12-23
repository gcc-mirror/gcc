// PR c++/43076

struct S;
template < typename > struct T
{
  template < typename >
  template < bool > struct T < S > // { dg-error "" }
  {
    void f () { // { dg-error "expected" }
