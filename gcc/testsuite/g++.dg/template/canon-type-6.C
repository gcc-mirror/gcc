// Contributed by Dodji Seketeli <dodji@redhat.com>
// Origin PR c++/39754
// { dg-do compile }

struct Foo {};
template<typename> struct A ;
template<typename T ,typename = A<T> > struct B { } ;

template<class W, class>
struct D
{
  typedef W X;
  A<void (Foo::*) (X)> a;
} ;

template<class Y>
struct E
{
  B<void (Foo::*) (Y)> b;
};
E < int > e ;

