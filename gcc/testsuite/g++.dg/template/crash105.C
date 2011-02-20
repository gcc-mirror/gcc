// PR c++/44118

template < typename > struct S;
template < typename > struct S < int >; // { dg-error "template" }
template < typename > struct S < int >
{
  void f ();
};

void
f ()
{
  S < int >::f (); // { dg-error "cannot call" }
}
