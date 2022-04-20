// PR c++/105233
// { dg-do compile { target c++14 } }

template <typename T>
constexpr T
foo (T x) noexcept
{
  bool a = __builtin_is_constant_evaluated ();
  T b = 4 * alignof (int);
  return x < b ? b : x;
}

template <typename T>
struct A { T a, b, c; };

template <typename T>
struct alignas (foo (sizeof (A<T>))) B { A<T> d; };

B<int> e;
