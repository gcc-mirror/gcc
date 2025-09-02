// PR c++/121068
// { dg-do compile { target c++26 } }

#include <new>

struct S
{
  constexpr S() = default;
  constexpr S(int x) : s(x) {}
  constexpr S(S&& x) : s(x.s) {}
  constexpr S& operator=(S&& x) { s = x.s; return *this; }
  unsigned char s;
};

constexpr
int foo()
{
  union { S a[20]; };
  new (&a) S[20](); // OK
  for (int i = 0; i < 20; ++i)
    a[i].~S();

  auto* sf = ::new(&a[2]) S(11);
  return 1;
}

static_assert(foo());

constexpr
int foo2()
{
  union { S a[20]; };
  new (&a) S[20]; // ILL-FORMED
  for (int i = 0; i < 20; ++i)
    a[i].~S();

  auto* sf = ::new(&a[2]) S(11);
  return 1;
}

static_assert(foo2());

auto p = foo2;
