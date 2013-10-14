// { dg-do compile }
// { dg-options "-std=c++11 -fgnu-tm" }

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
