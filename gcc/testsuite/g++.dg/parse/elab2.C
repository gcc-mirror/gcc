struct A {};

struct B
{
  typedef A T; // { dg-error "previous declaration" }
  friend struct T; // { dg-error "" }
};
