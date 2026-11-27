// C++26 P3861R1 - Pragmatic approach to standard structural types
// { dg-do compile { target c++20 } }

#if !__has_cpp_attribute(gnu::structural_type)
#error
#endif
#if !__has_attribute(structural_type)
#error
#endif
struct [[gnu::structural_type]] A { constexpr A () : a (0) {} private: int a; };
template <typename T>
struct [[__gnu__::__structural_type__]] B { constexpr B () : a (0) {} protected: T a; };
struct [[gnu::structural_type]] C { C () : a (0) {} protected: int a; };
template <typename T>
struct [[gnu::structural_type]] D { D () : a (0) {} private: T a; };
struct E { E (int x = 0) : a (x) {} private: int a; };
struct F { constexpr F (int x = 0) : a (x) {} int a; };
template <typename T>
struct __attribute__((__structural_type__)) G : private T { constexpr G () : g (0) {} int g; };
static_assert (__builtin_is_structural (A));
static_assert (__builtin_is_structural (B <int>));
static_assert (!__builtin_is_structural (B <E>));
static_assert (__builtin_is_structural (B <F>));
static_assert (!__builtin_is_structural (C));
static_assert (!__builtin_is_structural (D <int>));
static_assert (!__builtin_is_structural (D <E>));
static_assert (!__builtin_is_structural (D <F>));
static_assert (!__builtin_is_structural (E));
static_assert (__builtin_is_structural (F));
static_assert (__builtin_is_structural (G <F>));
static_assert (!__builtin_is_structural (G <E>));
template <auto A>
[[gnu::noipa]] void foo () {}

void
bar ()
{
  foo <A {}> ();
  foo <B <int> {}> ();
  foo <B <F> {}> ();
  foo <F {}> ();
  foo <G <F> {}> ();
}
