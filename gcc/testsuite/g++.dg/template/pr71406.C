// { dg-do compile }
// PR c++/71406 ICE with X::template Name

template < typename T >
struct C : T
{
  void foo () { this->C::template bar <>; }
};

template < typename T >
struct A
{ 
  template < void (T::*Fn) () > void f () {}
};

template < typename T > struct B : A < B < T > >
{ 
  void g ()
  { 
    this->B::template f < &B < T >::g > ();
  }
};

void Foo ()
{ 
  B < int > b;
  b.g ();
}
