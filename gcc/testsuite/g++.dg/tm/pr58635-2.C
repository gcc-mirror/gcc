// { dg-do compile { target c++11 } }
// { dg-options "-fgnu-tm" }

int
foo (void)
{
  return __transaction_atomic noexcept(false) (x); // { dg-error "was not declared in this scope" }
}
