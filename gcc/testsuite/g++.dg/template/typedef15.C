// Contributed by Dodji Seketeli <dodji@redhat.com>
// Origin: PR c++/26693
// { dg-do compile }

template<class T> struct C0;

struct Foo {
  typedef int TypedefedFoo;
  typedef C0<Foo> TypedefedC0;
};

template<class T>
struct C0
{
  typedef Foo TypedefedFoo;
  typename T::TypedefedC0::TypedefedFoo m;
};

template<class U>
struct C1
{
  typedef C0<Foo> TypedefedC0;
};

C0<C1<int> > c;
