// Contributed by Dodji Seketeli <dodji@redhat.com>
// Origin PR c++/40007
// { dg-do compile }

class A
{
  typedef int mytype; // { dg-error "'typedef int A::mytype' is private" }
};

template <class T>
class B : public A
{
};

template<class T>
class B<T*> : public A
{
  mytype mem; // { dg-error "within this context" }
};

B<int*> b;
