// { dg-do compile { target c++11 } }

namespace std {
  template <typename T> struct tuple_size;
  template <int, typename> struct tuple_element;
}

struct A { int a, b, c, d, e; };
struct B {};
struct C { int a, b; };
struct D { int a, b, c; static int d; };
struct E { int a : 1; int : 0; int : 2; int b : 1; int c : 3; int d : 4; };
typedef float V [[gnu::vector_size (16 * sizeof (float))]];
template <>
struct std::tuple_size <C> { static constexpr int value = 42; };

static_assert (__builtin_structured_binding_size (const A) == 5, "");
static_assert (__is_same_as (decltype (__builtin_structured_binding_size (A)), decltype (sizeof (int))), "");
static_assert (__builtin_structured_binding_size (B) == 0, "");
static_assert (__builtin_structured_binding_size (C) == 42, "");
static_assert (__builtin_structured_binding_size (A[17]) == 17, "");
static_assert (__builtin_structured_binding_size (C[6]) == 6, "");
static_assert (__builtin_structured_binding_size (volatile _Complex double) == 2, "");
static_assert (__builtin_structured_binding_size (V) == 16, "");
static_assert (__builtin_structured_binding_size (float [[gnu::vector_size (8 * sizeof (float))]]) == 8, "");
static_assert (__builtin_structured_binding_size (D) == 3, "");
static_assert (__builtin_structured_binding_size (E) == 4, "");

struct F {
  static short f[42];
  static_assert (__builtin_structured_binding_size (decltype (f)) == 42, "");
};

template <typename A, typename B, typename C, typename D,
	  typename E, typename F, typename G, typename H,
	  typename I, typename J>
void
foo ()
{
  static_assert (__builtin_structured_binding_size (const A) == 5, "");
  static_assert (__builtin_structured_binding_size (B) == 0, "");
  static_assert (__builtin_structured_binding_size (C) == 42, "");
  static_assert (__builtin_structured_binding_size (D) == 17, "");
  static_assert (__builtin_structured_binding_size (E) == 6, "");
  static_assert (__builtin_structured_binding_size (F) == 2, "");
  static_assert (__builtin_structured_binding_size (volatile G) == 16, "");
  static_assert (__builtin_structured_binding_size (H) == 8, "");
  static_assert (__builtin_structured_binding_size (I) == 3, "");
  static_assert (__builtin_structured_binding_size (J) == 4, "");
}

void
bar ()
{
  foo <A, B, C, A[17], C[6], _Complex double, V, float [[gnu::vector_size (8 * sizeof (float))]], D, E> ();
}
