// i++ is never an lvalue
void
f()
{
  bool i = 0;
  i++ = 3; // { dg-error "" }
  // { dg-warning "3:use of an operand of type .bool. in .operator\\+\\+. is deprecated" "" { target { ! c++17 } } .-1 }
}
