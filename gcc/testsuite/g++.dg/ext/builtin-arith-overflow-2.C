// { dg-do run }
// { dg-options "-O2" }

struct A { int i : 1; };
struct B { int j : 3; };

template <typename S>
int
foo (int x, int y)
{
  A a = {};
  S s = {};
  return __builtin_add_overflow_p (x, y, a.i) + 2 * __builtin_mul_overflow_p (x, y, s.j);
}

__attribute__((noinline, noclone)) int
bar (int x, int y)
{
  return foo <B> (x, y);
}

#if __cplusplus >= 201402L
template <typename S>
constexpr int
baz (int x, int y)
{
  A a = {};
  S s = {};
  return __builtin_add_overflow_p (x, y, a.i) + 2 * __builtin_mul_overflow_p (x, y, s.j);
}

constexpr int t1 = baz <B> (0, 0);
constexpr int t2 = baz <B> (1, -1);
constexpr int t3 = baz <B> (1, -4);
constexpr int t4 = baz <B> (-4, 4);
constexpr int t5 = baz <B> (4, 2);
static_assert (t1 == 0, "");
static_assert (t2 == 0, "");
static_assert (t3 == 1, "");
static_assert (t4 == 2, "");
static_assert (t5 == 3, "");
#endif

int
main ()
{
  if (bar (0, 0) != 0
      || bar (-1, 1) != 0
      || bar (-4, 1) != 1
      || bar (4, -4) != 2
      || bar (2, 4) != 3)
    __builtin_abort ();
}
