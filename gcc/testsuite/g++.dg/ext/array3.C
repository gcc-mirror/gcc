// PR c++/70709
// { dg-options "" }

struct A
{
  A (int);
};

struct B
{
  B () {} 
  A a[0];
};

struct C
{
  C () {} 
  B a[0];
};
