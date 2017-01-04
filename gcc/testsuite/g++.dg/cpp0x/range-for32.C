// PR c++/77545
// { dg-do compile { target c++11 } }
// { dg-options "-Wno-pedantic" }

template < typename T > struct A
{
  A ();
  ~A ();
  T t;
};

void f (A < int > a)
{
  for (auto x : (A<int>[]) { a })
    ;
}
