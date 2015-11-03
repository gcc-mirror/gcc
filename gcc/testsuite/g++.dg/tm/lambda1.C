// Test for lambda conversion.
// { dg-options "-fgnu-tm -std=c++14" }

void f(bool b)
{
  void (*p)() transaction_safe;

  p = []() transaction_safe {};
  p = []{};			// { dg-error "transaction_safe" }
}
