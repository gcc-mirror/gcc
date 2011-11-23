struct A {};

struct B
{
  typedef A T; // { dg-message "previous declaration" }
  friend struct T; // { dg-error "" }
};
