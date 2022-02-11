// P1272R4
// { dg-do compile { target c++14 } }

struct S { unsigned char a : 8, b : 5, c : 3, d, e; unsigned int f : 8, g : 24; };
struct T1 { unsigned char a : 1, : 7, b : 5, c : 3, d, e; unsigned int f : 8, g : 24; };
struct T2 { unsigned char a : 8, b : 1, : 4, c : 3, d, e; unsigned int f : 8, g : 24; };
struct T3 { unsigned char a : 8, b : 5, c : 1, : 2, d, e; unsigned int f : 8, g : 24; };
struct T4 { unsigned char a : 8, b : 5, c : 3, d, e; unsigned int f : 1, : 7, g : 24; };

constexpr bool
f1 ()
{
  T1 t = { 0, 0, 0, 0, 0, 0, 0 };
  S s = __builtin_bit_cast (S, t);
  return true;
}

constexpr bool
f2 ()
{
  T2 t = { 0, 0, 0, 0, 0, 0, 0 };
  S s = __builtin_bit_cast (S, t);
  return true;
}

constexpr bool
f3 ()
{
  T3 t = { 0, 0, 0, 0, 0, 0, 0 };
  S s = __builtin_bit_cast (S, t);
  return true;
}

constexpr bool
f4 ()
{
  T4 t = { 0, 0, 0, 0, 0, 0, 0 };
  S s = __builtin_bit_cast (S, t);	// { dg-error "accessing uninitialized byte" }
  return true;
}

constexpr bool
f5 ()
{
  T1 t = { 0, 0, 0, 0, 0, 0, 0 };
  S s = __builtin_bit_cast (S, t);
  unsigned char a = s.a;
  return true;
}

constexpr bool
f6 ()
{
  T2 t = { 0, 0, 0, 0, 0, 0, 0 };
  S s = __builtin_bit_cast (S, t);
  unsigned char b = s.b;
  return true;
}

constexpr bool
f7 ()
{
  T3 t = { 0, 0, 0, 0, 0, 0, 0 };
  S s = __builtin_bit_cast (S, t);
  unsigned char c = s.c;
  return true;
}

constexpr bool a = f1 ();
constexpr bool b = f2 ();
constexpr bool c = f3 ();
constexpr bool d = f4 ();
constexpr bool e = f5 ();	// { dg-error "accessing uninitialized member" }
constexpr bool f = f6 ();	// { dg-error "accessing uninitialized member" }
constexpr bool g = f7 ();	// { dg-error "accessing uninitialized member" }
