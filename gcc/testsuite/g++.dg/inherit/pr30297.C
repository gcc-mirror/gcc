// Regression test for ICE from PR c++/30297.

struct A
{
  int i;
};

extern "C" struct B : A
{
  A::i;
};
