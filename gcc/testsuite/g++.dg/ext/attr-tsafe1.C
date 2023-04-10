// PR c++/108795

template <typename T> void g (T x)
{
  struct C
  {
    __attribute__((transaction_safe)) ~C();
  };
  C::~C();			// { dg-error "" }
}
void f ()
{
  g (5);
}
