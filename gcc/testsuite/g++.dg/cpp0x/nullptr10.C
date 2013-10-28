// { dg-do compile }
// { dg-options "-std=c++11" }

// Test arithmetic operations

void fun()
{
  nullptr = 0;         // { dg-error "lvalue required as left operand" }
  nullptr + 2;         // { dg-error "invalid operands of types " }
  decltype(nullptr) mynull = 0;
  mynull = 1;          // { dg-error "cannot convert" }
  mynull = 0;
  mynull + 2;          // { dg-error "invalid operands of types " }
}
