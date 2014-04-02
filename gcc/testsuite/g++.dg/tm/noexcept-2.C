// { dg-do compile { target c++11 } }
// { dg-options "-fgnu-tm" }

// All of these must fail, because they are not constant expressions.
template<typename T> int foo(int x, T t)
{
  __transaction_atomic noexcept(t) { x++; }      /* { dg-error "not a constant" } */
  return __transaction_atomic noexcept(t) (x+1); /* { dg-error "not a constant" } */
}

int bar(int x)
{
  __transaction_atomic noexcept(x == 23) { x++; }      /* { dg-error "not a constant" } */
  return __transaction_atomic noexcept(x == 42) (x+1); /* { dg-error "not a constant" } */
}

int f(int x)
{
  return foo<bool>(x, true);
}
