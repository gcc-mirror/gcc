struct A {};

struct B
{
  typedef A T;
  friend struct T; // { dg-error "" }
};
