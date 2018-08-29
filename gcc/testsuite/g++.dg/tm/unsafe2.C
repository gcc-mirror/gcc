// Transaction-unsafe testcase from TM TS.
// { dg-options -fgnu-tm }

template<class T>
void f(T) transaction_safe;
template<>
void f(bool); // not transaction-safe

void g() transaction_safe
{
  f(42);			// OK
  f(true);			// { dg-error "unsafe" }
}
