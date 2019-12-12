// { dg-do run }
// { dg-options "-std=c++2a" }

extern "C" void abort ();
namespace std {
  constexpr inline bool
  is_constant_evaluated () noexcept
  {
    return __builtin_is_constant_evaluated ();
  }
}

int
main ()
{
  constexpr int a = 5;
  auto b = [] (int n) consteval { return n + a + std::is_constant_evaluated (); };
  int c = b (4);
  if (c != 10)
    abort ();
  auto d = [] () consteval { return a + std::is_constant_evaluated (); };
  int e = d ();
  if (e != 6)
    abort ();
  constexpr int f = d ();
  if (f != 6)
    abort ();
  static_assert (d () == 6);
}
