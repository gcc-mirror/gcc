// PR c++/79606
// { dg-do compile { target c++11 } }

struct A
{
  int i = 0;
};

template<int> struct B : A
{
  int j = this->i;
};

B<0> b;
