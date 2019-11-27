// { dg-do compile { target c++17 } }

int
fn (bool b)
{
  int r = 0;

  r += b++; // { dg-error "8:use of an operand of type .bool. in .operator\\+\\+. is forbidden in" }
  r += ++b; // { dg-error "10:use of an operand of type .bool. in .operator\\+\\+. is forbidden in" }
  r += b--; // { dg-error "8:use of an operand of type .bool. in .operator--. is forbidden" }
  r += --b; // { dg-error "10:use of an operand of type .bool. in .operator--. is forbidden" }

  return r;
}
