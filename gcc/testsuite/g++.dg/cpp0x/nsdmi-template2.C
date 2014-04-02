// PR c++/50614
// { dg-do compile { target c++11 } }
// { dg-options "-fcompare-debug" }

struct A
{
  int f ();
};

template <int> struct B : A
{
  int i = this->f ();
};

B<0> b;
