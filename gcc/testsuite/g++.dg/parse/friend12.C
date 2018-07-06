// PR c++/58650

struct A
{
  friend int i = 0;  // { dg-error "cannot be declared friend" }
};
