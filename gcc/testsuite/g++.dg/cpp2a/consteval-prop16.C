// P2564R3
// { dg-do compile { target c++20 } }
// Test unevaluated operands.

consteval int id (int i) { return i; }

constexpr int
f1 (auto i)
{
  // Unevaluated operand -> don't promote.
  auto p = sizeof (&id);
  (void) p;
  return i;
}

constexpr int
f2 (auto i)
{
  // Unevaluated operand -> don't promote.
  auto p = noexcept (id);
  (void) p;
  return i;
}

constexpr int
f3 (auto i)
{
  // Unevaluated operand -> don't promote.
  auto p = noexcept (id (i));
  (void) p;
  return i;
}

constexpr int
f4 (auto i)
{
  // Unevaluated operand -> don't promote.
  decltype(id) p;
  (void) p;
  return i;
}

constexpr int
f5 (auto i)
{
  // Unevaluated operand -> don't promote.
  __extension__ auto p = alignof (id (i));
  (void) p;
  return i;
}

constexpr int
f6 (auto i) requires requires { id (i); }
{
  return i;
}

void
g (int non_const)
{
  f1 (42);
  f1 (non_const);
  f2 (42);
  f2 (non_const);
  f3 (42);
  f3 (non_const);
  f4 (42);
  f4 (non_const);
  f5 (42);
  f5 (non_const);
  f6 (42);
  f6 (non_const);
}
