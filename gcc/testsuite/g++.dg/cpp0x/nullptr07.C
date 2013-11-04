// { dg-do compile }
// { dg-options "-std=c++11" }

// Test compare to int

void fun()
{
  int n = 0;
  if( n == nullptr ); // { dg-error "invalid operands of types " }
  const int m = 1;
  if( m == nullptr ); // { dg-error "invalid operands of types " }
  decltype(nullptr) mynull = 0;
  if( n == mynull ); // { dg-error "invalid operands of types " }
  if( m == mynull ); // { dg-error "invalid operands of types " }
}
