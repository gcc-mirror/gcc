// PR c++/80639
// { dg-do compile { target c++14 } }

template < typename > struct A;

struct B
{ 
  template < int > void m ();
  template < int > struct K { static void n (); };
  void p () { K < 0 >::n (); }
};

template <> struct A < B >
{ 
  using T = void (A::*)();
  template < int u > static constexpr T h = &B::m < u >; // { dg-error "cannot convert" }
};

template < int v > void B::K < v >::n ()
{ 
  using S = A < B >;
  S::h < 0 >;
}
