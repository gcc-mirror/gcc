// { dg-do compile { target { c++20 && { ilp32 || lp64 } } } }

struct A { signed char a, b, c, d, e, f; };
struct B {};
struct C { B a, b; short c; B d; };
struct D { int a : 4, b : 24, c : 4; };
struct E { B a, b; short c; };
struct F { B a; signed char b, c; B d; };

constexpr bool
f1 ()
{
  A a;
  a.c = 23; a.d = 42;
  C b = __builtin_bit_cast (C, a); // OK
  return false;
}

constexpr bool
f2 ()
{
  A a;
  a.a = 1; a.b = 2; a.c = 3; a.e = 4; a.f = 5;
  C b = __builtin_bit_cast (C, a);	// { dg-error "'__builtin_bit_cast' accessing uninitialized byte at offset 3" }
  return false;
}

constexpr bool
f3 ()
{
  D a;
  a.b = 1;
  F b = __builtin_bit_cast (F, a); // OK
  return false;
}

constexpr bool
f4 ()
{
  D a;
  a.b = 1; a.c = 2;
  E b = __builtin_bit_cast (E, a); // OK
  return false;
}

constexpr bool
f5 ()
{
  D a;
  a.b = 1;
  E b = __builtin_bit_cast (E, a);	// { dg-error "'__builtin_bit_cast' accessing uninitialized byte at offset 3" }
  return false;
}

constexpr bool
f6 ()
{
  D a;
  a.c = 1;
  E b = __builtin_bit_cast (E, a);	// { dg-error "'__builtin_bit_cast' accessing uninitialized byte at offset 2" }
  return false;
}

constexpr bool a = f1 ();
constexpr bool b = f2 ();
constexpr bool c = f3 ();
constexpr bool d = f4 ();
constexpr bool e = f5 ();
constexpr bool f = f6 ();
