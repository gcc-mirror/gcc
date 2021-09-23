// P1938R3
// { dg-do run { target c++20 } }
// { dg-options "" }

constexpr inline bool
is_constant_evaluated () noexcept
{
  if consteval { return true; } else { return false; }	// { dg-warning "'if consteval' only available with" "" { target c++20_only } }
}

template<int N> struct X { int v = N; };
X<is_constant_evaluated ()> x; // type X<true>
int y = 4;
int a = is_constant_evaluated () ? y : 1; // initializes a to 1
int b = is_constant_evaluated () ? 2 : y; // initializes b to 2
int c = y + (is_constant_evaluated () ? 2 : y); // initializes c to 2*y
int d = is_constant_evaluated (); // initializes d to 1
int e = d + is_constant_evaluated (); // initializes e to 1 + 0

struct false_type { static constexpr bool value = false; };
struct true_type { static constexpr bool value = true; };
template<class T, class U>
struct is_same : false_type {};
template<class T>
struct is_same<T, T> : true_type {};

constexpr int
foo (int x)
{
  const int n = is_constant_evaluated () ? 13 : 17; // n == 13
  int m = is_constant_evaluated () ? 13 : 17; // m might be 13 or 17 (see below)
  char arr[n] = {}; // char[13]
  return m + sizeof (arr) + x;
}

constexpr int
bar ()
{
  const int n = is_constant_evaluated() ? 13 : 17;
  X<n> x1;
  X<is_constant_evaluated() ? 13 : 17> x2;
  static_assert (is_same<decltype (x1), decltype (x2)>::value, "x1/x2's type");
  return x1.v + x2.v;
}

int p = foo (0); // m == 13; initialized to 26
int q = p + foo (0); // m == 17 for this call; initialized to 56
static_assert (bar () == 26, "bar");

struct S { int a, b; };

S s = { is_constant_evaluated () ? 2 : 3, y };
S t = { is_constant_evaluated () ? 2 : 3, 4 };

static_assert (is_same<decltype (x), X<true> >::value, "x's type");

int
main ()
{
  if (a != 1 || b != 2 || c != 8 || d != 1 || e != 1 || p != 26 || q != 56)
    __builtin_abort ();
  if (s.a != 3 || s.b != 4 || t.a != 2 || t.b != 4)
    __builtin_abort ();
  if (foo (y) != 34)
    __builtin_abort ();
#if __cplusplus >= 201703L
  if constexpr (foo (0) != 26)
    __builtin_abort ();
#endif
  constexpr int w = foo (0);
  if (w != 26)
    __builtin_abort ();
}
