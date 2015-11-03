// { dg-options "-fgnu-tm" }

void fn(int) transaction_safe;
void fn(double);

template <class T> void f(T t) transaction_safe
{
  fn(t);			// { dg-error "double" }
}

void g()
{
  f(42); // OK
  f(3.14);
}
