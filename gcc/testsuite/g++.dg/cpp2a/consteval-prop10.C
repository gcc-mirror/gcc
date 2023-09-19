// P2564R3
// { dg-do compile { target c++20 } }
// Test default arguments.

consteval int id (int i) { return i; }

template<typename>
constexpr int
f1 (int i = id (42))
{
  return i;
}

int non_const; // { dg-message ".int non_const. is not const" }

template<typename>
constexpr int
f2 (int i = id (non_const))
{
  return i;
}

constexpr int
f3 (auto)
{
  return f2<int>(); // { dg-message "contains an immediate-escalating expression .id\\(non_const\\)." }
}

auto a = &f3<int>; // { dg-error "taking address of an immediate function" }

void
g (int i)
{
  f1<int> (42);
  f1<int> (i);
  f1<int> ();
  f2<int> (42);
  f2<int> (i);
  f2<int> (); // { dg-error "call to consteval function .id\\(non_const\\). is not a constant expression" }
// { dg-error ".non_const. is not usable in a constant expression" "" { target *-*-* } .-1 }
}
