// PR c++/80186

template < class T, class > struct A
{
  A ();
  A (A &);
  A (A < T, T >);  // { dg-error "invalid constructor" }
};

void f () 
{
  A < int, int > (A < int, int >());  // { dg-error "cannot bind" "" { target c++14_down } }
}
