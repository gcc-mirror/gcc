// P1668R1: Permit unevaluated inline asm in constexpr functions
// PR c++/91346
// { dg-do compile { target c++2a } }

constexpr int
foo (bool b)
{
  if (b)
    return 42;
  asm (""); // { dg-error "inline assembly is not a constant expression" }
// { dg-message "only unevaluated inline assembly" "" { target *-*-* } .-1 }
  return -1;
}

constexpr int i = foo (true);
static_assert(i == 42, "");
constexpr int j = foo (false); // { dg-message "in .constexpr. expansion of" }
