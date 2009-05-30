// Contributed by Dodji Seketeli <dodji@redhat.com>
// Origin PR c++/40007
// { dg-do compile }

class A
{
  typedef int mytype; // { dg-error "'typedef int A::mytype' is private" "" { xfail *-*-* } }
};

template <class T>
class B : public A
{
};

template<class T>
class B<T*> : public A
{ // { dg-error "within this context" "" { xfail *-*-* } }
  mytype mem;
};

B<int*> b;
