// PR c++/29435

template < class T > struct A {};
template < int> void g()
{
  sizeof (A < int>);
}

template < class T > struct B;
template < int> void f()
{
  sizeof (B<int>); // { dg-error "3:invalid application of .sizeof. to incomplete type" }
}

