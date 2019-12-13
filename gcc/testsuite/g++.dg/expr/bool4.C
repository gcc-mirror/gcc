// { dg-do compile }
// make sure that a typedef for a bool will have the
//  the same results as a bool itself.


typedef volatile bool my_bool;
int main()
{
  my_bool b = false;
  b--; // { dg-error "3:use of an operand of type .bool. in .operator\\-\\-. is forbidden" }
  // { dg-warning ".volatile.-qualified type is deprecated" "" { target c++2a } .-1 }
  return 0;
}
