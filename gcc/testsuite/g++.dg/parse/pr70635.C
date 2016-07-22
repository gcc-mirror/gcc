// PR c++/70635
// { dg-options "-fpermissive -w" }

template < typename T > 
struct A
{
  struct B;
  typedef typename B::type type;
};

template < typename T > 
struct A < T >::B
{
  typedef typename A < type >::type type;  // { dg-error "type" }
  type Foo ();
};

template < typename T > 
typename A < T >::B::type
A < T >::B::Foo ()
{
  return 0;
}

template class A<int>;
