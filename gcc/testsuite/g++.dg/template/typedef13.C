// Contributed by Dodji Seketeli <dodji@redhat.com>
// Origin: PR c++/26693
// { dg-do compile }

class A
{
  typedef int mytype; // { dg-message "private" }
};

template <class T> class B : public A
{
  mytype mem; // { dg-error "within this context"  }
};

B<int> b; // { dg-message "required from here" }

