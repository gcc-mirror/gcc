// { dg-do compile { target c++11 } }
// { dg-options "-fgnu-tm" }

int
foo (void)
{
  return __transaction_atomic noexcept(false) (false);
}

int
bar (int i)
{
  return __transaction_atomic noexcept(false) (i);
}
