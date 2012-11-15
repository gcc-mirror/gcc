// PR c++/43630

template < typename > struct A;

template < typename > struct A < int > // { dg-error "not used|template\\-parameter|declaration" }
{
  int i;
  int f ();
};

int A < int >::f () // { dg-error "incomplete type" }
{
  return i;
}
