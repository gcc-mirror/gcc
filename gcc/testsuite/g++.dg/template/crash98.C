// PR c++/43630

template < typename > struct A; // { dg-error "declaration" }

template < typename > struct A < int > // { dg-error "not used|template\\-parameter" }
{
  int i;
  int f ();
};

int A < int >::f () // { dg-error "incomplete type" }
{
  return i;
}
