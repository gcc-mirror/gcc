// { dg-do compile { target c++11 } }

namespace N
{ 
  template < typename T > class A
  { 
    template < T > friend class B;  // { dg-error "not a valid type" }
  };

  A < float > a;
}
