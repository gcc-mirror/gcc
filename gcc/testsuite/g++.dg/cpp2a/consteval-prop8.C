// P2564R3
// { dg-do compile { target c++20 } }
// { dg-options "-Wno-c++23-extensions" }

consteval int zero (int)
{
  return 0;
}

struct A {
  // A::A(auto) promoted to consteval.
  constexpr A(auto i) { zero (i); }
};

// 'f1<int>' is an immediate function because its body contains a call to an
// immediate constructor 'A<int>' and that call is not a constant expression
constexpr void
f1 (auto i)
{
  A a{i};
}

// 'f2<int>' is an immediate function because its body contains a call to an
// immediate constructor 'A<int>' and that call is not a constant expression
constexpr void
f2 (auto i)
{
  A a{i};
}

void
f3 (int i)
{
  A a{i}; // { dg-error "not a constant expression" }
}

inline void
f7 (int i)
{
  A a{i}; // { dg-error "not a constant expression" }
}

constexpr void
f8 (int i)
{
  A a{i}; // { dg-error "not a constant expression" }
}

/* "An expression or conversion is immediate-escalating if it is not initially
   in an immediate function context" but this one is, so we do *not* promote
   f4 to consteval.  */
constexpr void
f4 (auto i)
{
  if consteval {
    A a{i};
  }
}

constexpr void
f5 (auto i)
{
  if not consteval {
    (void) 0;
  } else {
    A a{i};
  }
}

void
f6 (int x)
{
  f1 (0);
  f1 (x); // { dg-error "not a constant expression" }
  f2 (0);
  f2 (x); // { dg-error "not a constant expression" }
  f3 (0);
  f4 (x);
  f4 (0);
  f5 (x);
  f5 (0);
}
