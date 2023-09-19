// P2564R3
// { dg-do compile { target c++20 } }
// { dg-options "-fno-immediate-escalation" }

consteval int id(int i) { return i; }

constexpr int
f (auto i)
{
  return id (i); // { dg-error "not a constant expression" }
}

int
g ()
{
  return f (42);
}
