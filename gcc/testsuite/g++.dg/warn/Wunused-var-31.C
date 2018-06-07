// PR c++/85108
// { dg-do compile { target c++17 } }
// { dg-options "-Wunused-but-set-variable" }

int
main ()
{
  auto constexpr add = [] (auto a, auto b) { return a + b; };	// { dg-bogus "set but not used" }
  auto test_lambda = [&] () { return add (2, 2); };
  return test_lambda () - 4;
}
