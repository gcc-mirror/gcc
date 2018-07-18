// PR c++/71451
// { dg-do compile { target c++11 } }

template < int > struct A;

template < typename ... T >
struct B
{ 
  template < typename A < T::value >::type > void foo ();  // { dg-error "parameter packs" }
};

int main ()
{ 
  B < int > t;
  t.foo ();
  return 0;
}
