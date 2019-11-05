// { dg-do run { target c++2a } }

#include <compare>

#define assert(X) do { if (!(X)) __builtin_abort(); } while(0)

void f(){}
void g(){}

template <class T, class U, class R>
constexpr bool check(T a, U b, R expected)
{
  auto r = a <=> b;
  static_assert (__is_same_as (decltype (r), R));
  return r == expected;
}

int main()
{
  static_assert (check (1, 2, std::strong_ordering::less));

  enum E1 { a = 0 };
  static_assert (check (a, 1, std::strong_ordering::less));

  enum class E2 { a, b };
  static_assert (check (E2::a, E2::b, std::strong_ordering::less));

  int ar[2];
  static_assert (check (&ar[1], &ar[0], std::strong_ordering::greater));

  static_assert (check (3.14, 3.14, std::partial_ordering::equivalent));

  // GCC doesn't consider &f == &g to be a constant expression (PR 69681)
  assert (check (&f, &g, std::strong_equality::nonequal));

  struct A { int i; int j; };
  static_assert (check (&A::i, &A::j, std::strong_equality::nonequal));

  struct A2 { void f(); };
  static_assert (check (&A2::f, &A2::f, std::strong_equality::equal));
}
