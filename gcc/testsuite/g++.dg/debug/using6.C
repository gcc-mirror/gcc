// PR c++/51189
// { dg-do compile }

struct A
{
  int i1, i2, i3, i4, i5, i6;
};

struct B : A
{
  using A::i1;
  using A::i2;
  using A::i3;
  using A::i4;
  using A::i5;
  using A::i6;
};

struct C : B
{
  using B::i1;
};
