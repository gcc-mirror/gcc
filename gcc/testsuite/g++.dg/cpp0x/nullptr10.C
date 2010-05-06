// { dg-do compile }
// { dg-options "-std=c++0x" }

// Test arithmetic operations

void fun()
{
  nullptr = 0;         // { dg-error "lvalue required as left operand" }
  nullptr + 2;         // { dg-error "invalid operands of types " }
}
