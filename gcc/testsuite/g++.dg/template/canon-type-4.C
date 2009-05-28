// Contributed by Dodji Seketeli <dodji@redhat.com>
// Origin PR c++/39754
// { dg-do "compile" }

template<typename> struct A ;
template<typename T ,typename = A<T> > struct B { } ;

template<class W, class>
struct D
{
  typedef W X;
  A<X[2]> a;
} ;

template<class Y>
struct E
{
  B<Y[2]> b;
};

E < int > e;

