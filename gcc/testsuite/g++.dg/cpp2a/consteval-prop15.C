// P2564R3
// { dg-do compile { target c++20 } }
// { dg-options "-Wno-c++23-extensions" }

consteval int id (int i) { return i; }

constexpr int
f1 (auto i)
{
  auto p = &id; // { dg-message "promoted to an immediate function because its body contains an immediate-escalating expression .id." }
  (void) p;
  return i;
}

constexpr int
f2 (auto i)
{
  return f1 (i);
}

constexpr int
f3 (auto i)
{
  return f2 (i);
}

constexpr int
f4 (auto i)
{
  return f3 (i);
}

constexpr int
f5 (auto i)
{
  return f4 (i); // { dg-message "promoted to an immediate function because its body contains an immediate-escalating expression .f4<int>\\(i\\)." }
}

constexpr int
f6 (auto)
{
  // This call is a constant expression, so don't promote f6.
  return f4 (42);
}

constexpr int
f7 (auto i)
{
  if consteval {
    auto p = &id;
    (void) p;
  }
  return i;
}

constexpr int
f8 (auto i)
{
  if not consteval {
    (void) 0;
  } else {
    auto p = &id;
    (void) p;
  }
  return i;
}

constexpr int
f9 (auto i)
{
  if consteval {
    return id(i);
  }
  return i;
}

constexpr int
f10 (auto i)
{
  if not consteval {
    (void) 0;
  } else {
    return id(i);
  }
  return i;
}

void
g (int non_const)
{
  f1 (42);
  f1 (non_const); // { dg-error "call to consteval function .f1<int>\\(non_const\\). is not a constant expression" }
// { dg-error ".non_const. is not a constant expression" "" { target *-*-* } .-1 }
  f5 (42);
  f5 (non_const); // { dg-error "call to consteval function .f5<int>\\(non_const\\). is not a constant expression" }
// { dg-error ".non_const. is not a constant expression" "" { target *-*-* } .-1 }
  f6 (42);
  f6 (non_const);
  f7 (42);
  f7 (non_const);
  f8 (42);
  f8 (non_const);
  f9 (42);
  f9 (non_const);
  f10 (42);
  f10 (non_const);
}
