// PR libstdc++/123326
// { dg-do compile { target c++20 } }
// Key method is S::foo(int).  While it is constexpr and thus implicitly
// inline, it has gnu::gnu_inline attribute and so the method is only
// inlined but not emitted out of line in the current TU.
// { dg-final { scan-assembler-not "_ZTS1S:" } }
// { dg-final { scan-assembler-not "_ZTV1S:" } }
// { dg-final { scan-assembler-not "_ZTI1S:" } }

struct S {
  [[gnu::gnu_inline]] constexpr virtual int foo (int x) { return x + 42; }
  [[gnu::gnu_inline]] constexpr virtual ~S () {}
};
static_assert (S {}.foo (0) == 42);

int
bar (S s, int x)
{
  return s.foo (x);
}

auto q = &S::foo;

int
plugh (S &s, int x)
{
  return s.foo (x);
}

S s;
