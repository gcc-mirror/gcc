// { dg-do compile { target c++11 } }
// { dg-options "-fgnu-tm" }

void foo()
{
  __transaction_atomic noexcept(false) {}
}
