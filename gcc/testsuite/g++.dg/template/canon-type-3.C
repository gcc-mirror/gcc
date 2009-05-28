// Contributed by Dodji Seketeli <dodji@redhat.com>
// Origin PR c++/39754
// { dg-do "compile" }

template<typename> struct A ;
template<typename T , typename = A<T> > struct B { } ;
template<class W , class > struct D
{
  typedef W X ;
  typedef X (FP) ();
  A<FP&> a ;
} ;

template < class Y > struct E
{
  typedef Y (FP) ();
  B<FP&> b ;
} ;
E < int > e ;

