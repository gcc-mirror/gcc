// { dg-do compile { target c++11 } }
// { dg-options "" }
// { dg-add-options float16 }
// { dg-add-options float32 }
// { dg-add-options float64 }
// { dg-add-options float128 }

struct A { int a, b, c; };
class B { static int a; private: static int b; public: int c; };
struct C { C () {} int a, b, c; };
struct D { explicit D (int) {} int a, b, c; };
struct E : public A { int d, e, f; };
struct F : public C { using C::C; int d, e, f; };
class G { int a, b; };
struct H { private: int a, b; };
struct I { protected: int a, b; };
struct J { int a, b; void foo (); };
struct K { int a, b; virtual void foo (); };
struct L : virtual public A { int d, e; };
struct M : protected A { int d, e; };
struct N : private A { int d, e; };
struct O { O () = delete; int a, b, c; };
struct P { P () = default; int a, b, c; };
struct Q { Q (); Q (const Q &); int a, b, c; };
struct R { R (); R (const R &); R (R &&) = default; int a, b, c; };
struct S { S (); ~S (); int a, b, c; };
struct T { T (); ~T () = default; int a, b, c; };
struct U { U (); U (const U &) = default; int a, b, c; };
struct V { V () = default; V (const V &); int a, b, c; };
enum W { W1 };
enum class X : int { X1 };
struct Y { int g; int foo (int); };
struct Z;
struct AA { Q a; Q b; };
struct AB { Q a; Q b; ~AB () = default; };
struct AC { Q a; Q b; ~AC () {} };
struct AD : public Q {};
struct AE : public Q { ~AE () = default; };
struct AF : public Q { ~AF () {} };

#define SA(X) static_assert ((X), #X)

SA (!__builtin_is_implicit_lifetime (void));
SA (!__builtin_is_implicit_lifetime (const void));
SA (!__builtin_is_implicit_lifetime (volatile void));
SA (__builtin_is_implicit_lifetime (char));
SA (__builtin_is_implicit_lifetime (signed char));
SA (__builtin_is_implicit_lifetime (const unsigned char));
SA (__builtin_is_implicit_lifetime (short));
SA (__builtin_is_implicit_lifetime (volatile unsigned short));
SA (__builtin_is_implicit_lifetime (int));
SA (__builtin_is_implicit_lifetime (unsigned int));
SA (__builtin_is_implicit_lifetime (const volatile long));
SA (__builtin_is_implicit_lifetime (unsigned long));
SA (__builtin_is_implicit_lifetime (long long));
SA (__builtin_is_implicit_lifetime (unsigned long long));
#ifdef __SIZEOF_INT128__
SA (__builtin_is_implicit_lifetime (__int128));
SA (__builtin_is_implicit_lifetime (unsigned __int128));
#endif
SA (__builtin_is_implicit_lifetime (float));
SA (__builtin_is_implicit_lifetime (double));
SA (__builtin_is_implicit_lifetime (long double volatile));
#ifdef __STDCPP_FLOAT16_T__
SA (__builtin_is_implicit_lifetime (_Float16));
#endif
#ifdef __STDCPP_FLOAT32_T__
SA (__builtin_is_implicit_lifetime (_Float32));
#endif
#ifdef __STDCPP_FLOAT64_T__
SA (__builtin_is_implicit_lifetime (const _Float64));
#endif
#ifdef __STDCPP_FLOAT128_T__
SA (__builtin_is_implicit_lifetime (_Float128));
#endif
#ifdef __STDCPP_BFLOAT16_T__
SA (__builtin_is_implicit_lifetime (decltype(0.bf16)));
#endif
SA (__builtin_is_implicit_lifetime (W));
SA (__builtin_is_implicit_lifetime (const volatile X));
SA (__builtin_is_implicit_lifetime (int *));
SA (__builtin_is_implicit_lifetime (int (*) (int)));
SA (__builtin_is_implicit_lifetime (int (Y::*)));
SA (__builtin_is_implicit_lifetime (int (Y::*) (int)));
SA (!__builtin_is_implicit_lifetime (int &));
SA (!__builtin_is_implicit_lifetime (char &&));
SA (__builtin_is_implicit_lifetime (int []));
SA (!__builtin_is_implicit_lifetime (int [0]));
SA (__builtin_is_implicit_lifetime (int [1]));
SA (__builtin_is_implicit_lifetime (const Y [42]));
SA (!__builtin_is_implicit_lifetime (int ()));
SA (!__builtin_is_implicit_lifetime (int () &));
SA (!__builtin_is_implicit_lifetime (int () const));
SA (!__builtin_is_implicit_lifetime (int (&) ()));
SA (!__builtin_is_implicit_lifetime (Z));		// { dg-error "invalid use of incomplete type 'struct Z'" }
SA (__builtin_is_implicit_lifetime (Z []));
SA (__builtin_is_implicit_lifetime (Z [5]));
SA (__builtin_is_implicit_lifetime (A));
SA (__builtin_is_implicit_lifetime (B));
SA (__builtin_is_implicit_lifetime (C));
SA (__builtin_is_implicit_lifetime (D));
SA (__builtin_is_implicit_lifetime (E));
SA (__builtin_is_implicit_lifetime (F));
SA (__builtin_is_implicit_lifetime (G));
SA (__builtin_is_implicit_lifetime (H));
SA (__builtin_is_implicit_lifetime (I));
SA (__builtin_is_implicit_lifetime (J));
SA (!__builtin_is_implicit_lifetime (K));
SA (!__builtin_is_implicit_lifetime (L));
SA (__builtin_is_implicit_lifetime (M));
SA (__builtin_is_implicit_lifetime (N));
SA (__builtin_is_implicit_lifetime (O));
SA (__builtin_is_implicit_lifetime (P));
SA (!__builtin_is_implicit_lifetime (Q));
SA (__builtin_is_implicit_lifetime (R));
SA (!__builtin_is_implicit_lifetime (S));
SA (__builtin_is_implicit_lifetime (S [3]));
SA (__builtin_is_implicit_lifetime (T));
SA (__builtin_is_implicit_lifetime (U));
SA (__builtin_is_implicit_lifetime (V));
SA (__builtin_is_implicit_lifetime (_Complex double));
SA (__builtin_is_implicit_lifetime (int [[gnu::vector_size (4 * sizeof (int))]]));
SA (__builtin_is_implicit_lifetime (AA));
SA (__builtin_is_implicit_lifetime (AB));
SA (!__builtin_is_implicit_lifetime (AC));
#if __cplusplus >= 201703L
SA (__builtin_is_implicit_lifetime (AD));
SA (__builtin_is_implicit_lifetime (AE));
#else
SA (!__builtin_is_implicit_lifetime (AD));
SA (!__builtin_is_implicit_lifetime (AE));
#endif
SA (!__builtin_is_implicit_lifetime (AF));

void
foo (int n)
{
  SA (__builtin_is_implicit_lifetime (char [n]));
}
