// Contributed by Dodji Seketeli <dodji@redhat.com>
// Origin: PR c++/26693
// { dg-do compile }

template <class T>
struct A
{
  typedef int mytype;

  void
  foo ()
  {
    mytype v = ~static_cast<mytype> (0);
  }
};

