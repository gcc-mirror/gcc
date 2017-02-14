// Test for static_cast.
// { dg-options -std=c++1z }

void f()
{
  typedef void (*P)();
  typedef void (*NP)() noexcept;

  P p;
  NP np;

  static_cast<P>(np);
  static_cast<NP>(p);		// { dg-error "" }
}
