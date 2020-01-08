// Test for static_cast.
// { dg-do compile { target c++17 } }

void f()
{
  typedef void (*P)();
  typedef void (*NP)() noexcept;

  P p;
  NP np;

  static_cast<P>(np);
  static_cast<NP>(p);		// { dg-error "3:invalid .static_cast." }
}
