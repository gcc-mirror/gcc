// P1668R1: Permit unevaluated inline asm in constexpr functions
// PR c++/91346
// { dg-do compile { target c++14 } }
// { dg-options "" }

constexpr int
foo (int a, int b)
{
  if (__builtin_is_constant_evaluated ())
    return a + b;
  asm (""); // { dg-warning ".asm. in .constexpr. function only available with" "" { target c++17_down } }
  return a;
}
