// Test for transaction-safety conversion in deduction.
// { dg-options "-fgnu-tm" }

void fn(int) transaction_safe;
void fn();

template <class T> void f(void(*)(T));
template <class T> void f2(void(*)(T) transaction_safe);

void g()
{
  f(fn);
}
