// { dg-do compile }
// { dg-options "-std=c++11 -fgnu-tm" }

int
foo (void)
{
  return __transaction_atomic noexcept(false) (x); // { dg-error "was not declared in this scope" }
}
