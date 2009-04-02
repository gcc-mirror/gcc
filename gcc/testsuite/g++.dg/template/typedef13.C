// Contributed by Dodji Seketeli <dodji@redhat.com>
// Origin: PR c++/26693
// { dg-do compile }

class A
{
  typedef int mytype; // { dg-error "typedef int A::mytype' is private" }
};

template <class T> class B : public A
{ // { dg-error "within this context"  }
  mytype mem;
};

B<int> b; // { dg-message "instantiated from here" }

