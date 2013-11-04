// PR c++/50614
// { dg-options "-std=c++11 -fcompare-debug" }

struct A
{
  int f ();
};

template <int> struct B : A
{
  int i = this->f ();
};

B<0> b;
