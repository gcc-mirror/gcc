// P0847R7
// { dg-do compile { target c++23 } }

// diagnose mutation of lambda capture when called with a deduced as const explicit object parameter

void test()
{
  auto f0 = [n = 5](this auto){ n = 10; }; // { dg-bogus {assignment of read-only variable} }
  auto f1 = [n = 5](this auto const){ n = 10; }; // { dg-error {assignment of read-only variable} }
  auto f2 = [n = 5](this auto&){ n = 10; };  // { dg-error {assignment of read-only variable} }
  auto f3 = [n = 5](this auto const&){ n = 10; }; // { dg-error {assignment of read-only variable} }
  auto f4 = [n = 5](this auto&&){ n = 10; };  // { dg-error {assignment of read-only variable} }

  static_cast<decltype(f0) const&>(f0)();
  f1();
  static_cast<decltype(f2) const&>(f2)();
  f3();
  static_cast<decltype(f4) const&>(f4)();
}

