// PR c++/29571

struct A
{
  static const int i = 0/0 + ""; // { dg-warning "division by zero" }
  // { dg-error "field initializer is not constant|not a constant-expression" "" { target *-*-* } 5 }
  static const int j = int(i);
};
