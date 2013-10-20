// { dg-do compile }
// { dg-options "-std=c++11 -fgnu-tm" }

void foo()
{
  __transaction_atomic noexcept(false) {}
}
