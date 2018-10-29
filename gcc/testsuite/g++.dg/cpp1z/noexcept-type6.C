// Test for lambda conversion.
// { dg-do compile { target c++17 } }

void f()
{
  auto l = []() noexcept { return 0; };
  int (*p)() noexcept = l;
  int (*q)() = l;

  auto l2 = []{ return 0; };
  p = l2;			// { dg-error "" }
  q = l2;
}
