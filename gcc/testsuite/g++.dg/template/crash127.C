// PR c++/71440

struct A 
{
  void f () {}
};

typedef void (A::*Ptr) ();

template < Ptr > struct B {};

template < class T > 
struct C : public A
{
  void bar ()
  {
    B < &A::A > b;  // { dg-error "taking address of constructor 'A::A" "" { target c++98_only } }
    // { dg-error "taking address of constructor 'constexpr A::A" "" { target c++11 } .-1 }
    // { dg-error "template argument 1 is invalid" "" { target *-*-* } .-2 }
  }
};
