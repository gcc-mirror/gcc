// Contributed by Dodji Seketeli <dodji@redhat.com>
// Origin: Jason Merrill <jason@redhat.com>, PR c++/26693
// { dg-do compile }

class A
{
     protected:
           typedef int mytype;
};

template <class T> class B;

class C: public A
{
      template <class T> friend class B;
};

template <class T> class B
{
      C::mytype mem;
};

B<int> b;
