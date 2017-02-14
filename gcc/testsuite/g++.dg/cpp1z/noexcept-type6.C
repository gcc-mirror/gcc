// Test for lambda conversion.
// { dg-options -std=c++1z }

void f()
{
  auto l = []() noexcept { return 0; };
  int (*p)() noexcept = l;
  int (*q)() = l;

  auto l2 = []{ return 0; };
  p = l2;			// { dg-error "" }
  q = l2;
}
