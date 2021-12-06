// P1272R4
// { dg-do compile { target c++14 } }

namespace std
{
  enum class byte : unsigned char {};
}

struct S { unsigned char a[2]; alignas(sizeof 0) int b; };
struct T { char a; alignas(sizeof 0) int b; };
struct U { char a : 1; char : 6; char b : 1; };
struct V { int a; S b; };
struct W { unsigned a; T b; };

constexpr bool
f1 ()
{
  T t = { 1, 2 };
  S s = __builtin_bit_cast (S, t);
  return s.a[0] == 1;
}

constexpr bool
f2 ()
{
  U u = { 0, 0 };
  unsigned char a = __builtin_bit_cast (unsigned char, u);
  return true;
}

constexpr bool
f3 ()
{
  T t = { 1, 2 };
  S s = __builtin_bit_cast (S, t);
  return s.a[1] == 0;
}

constexpr bool
f4 ()
{
  U u = { 0, 0 };
  unsigned char a = __builtin_bit_cast (unsigned char, u);
  return a == 0;		// { dg-error "is not a constant expression" }
}

constexpr bool
f5 ()
{
  W t = { 1, 2 };
  V s = __builtin_bit_cast (V, t);
  return s.b.a[0] == 1;
}

constexpr bool
f6 ()
{
  W t = { 1, 2 };
  V s = __builtin_bit_cast (V, t);
  return s.b.a[1] == 1;
}

constexpr bool a = f1 ();
constexpr bool b = f2 ();
constexpr bool c = f3 ();	// { dg-error "accessing uninitialized array element" }
constexpr bool d = f4 ();
constexpr bool e = f5 ();
constexpr bool f = f6 ();	// { dg-error "accessing uninitialized array element" }
