// P2564R3
// { dg-do compile { target c++20 } }
// Test more CALL_EXPRs in a function, some of which are escalating.

consteval int id (int i) { return i; }
constexpr int neg (int i) { return -i; }
constexpr int foo (auto i) { return id (i); }

constexpr int
f1 (auto i)
{
  auto x = id (i);  // { dg-message "promoted to an immediate function because its body contains an immediate-escalating expression .id\\(i\\)." }
  auto y = neg (i);
  return x + y;
}

constexpr int
f2 (auto i)
{
  return neg (id (i)); // { dg-message "promoted to an immediate function because its body contains an immediate-escalating expression .id\\(i\\)." }
}

constexpr int
f3 (auto i)
{
  auto x = i + neg (neg (neg (id (neg (neg (i)))))); // { dg-message "promoted to an immediate function because its body contains an immediate-escalating expression .id\\(neg\\(neg\\(i\\)\\)\\)." }
  return x;
}

constexpr int
f4 (auto i)
{
  return i + neg ((id (2 * i) + neg (i)) / 2); // { dg-message "promoted to an immediate function because its body contains an immediate-escalating expression .id\\(\\(i \\* 2\\)\\)." }
}

constexpr int
f5 (auto i)
{
  (void) neg (i);
  (void) neg (i);
  (void) neg (i);
  (void) neg (i);
  (void) neg (i);
  (void) neg (i);
  (void) +id (i); // { dg-message "promoted to an immediate function because its body contains an immediate-escalating expression .id\\(i\\)." }
  (void) neg (i);
  return i;
}

constexpr int
f6 (auto i)
{
  auto x = neg (i + foo (i)); // { dg-message "promoted to an immediate function because its body contains an immediate-escalating expression .foo<int>\\(i\\)." }
  return x;
}

void
g (int i)
{
  f1 (i); // { dg-error "call to consteval function .f1<int>\\(i\\). is not a constant expression" }
// { dg-error ".i. is not a constant expression" "" { target *-*-* } .-1 }
  f1 (42);
  f2 (i); // { dg-error "call to consteval function .f2<int>\\(i\\). is not a constant expression" }
// { dg-error ".i. is not a constant expression" "" { target *-*-* } .-1 }
  f2 (42);
  f3 (i); // { dg-error "call to consteval function .f3<int>\\(i\\). is not a constant expression" }
// { dg-error ".i. is not a constant expression" "" { target *-*-* } .-1 }
  f3 (42);
  f4 (i); // { dg-error "call to consteval function .f4<int>\\(i\\). is not a constant expression" }
// { dg-error ".i. is not a constant expression" "" { target *-*-* } .-1 }
  f4 (42);
  f5 (i); // { dg-error "call to consteval function .f5<int>\\(i\\). is not a constant expression" }
// { dg-error ".i. is not a constant expression" "" { target *-*-* } .-1 }
  f5 (42);
  f6 (i); // { dg-error "call to consteval function .f6<int>\\(i\\). is not a constant expression" }
// { dg-error ".i. is not a constant expression" "" { target *-*-* } .-1 }
  f6 (42);
}
