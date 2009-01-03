// PR c++/38698

struct A
{
  int i;
};

A a({1,2});

union U
{
  int i,j;
};

U u({1,2});
