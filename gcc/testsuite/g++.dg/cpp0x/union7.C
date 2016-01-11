// PR c++/69131
// { dg-do compile { target c++11 } }

struct X
{
  ~X() {}
};

union U
{
  X x;
  ~U() {}
};

U u;
