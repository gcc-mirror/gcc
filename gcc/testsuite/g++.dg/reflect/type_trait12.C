// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// { dg-add-options float16 }
// { dg-add-options float32 }
// { dg-add-options float64 }
// { dg-add-options float128 }
// Test reflection type traits [meta.reflection.traits], type properties.

#include <functional>
#include <meta>

using namespace std::meta;

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

static_assert (!is_implicit_lifetime_type (^^void));
static_assert (!is_implicit_lifetime_type (^^const void));
static_assert (!is_implicit_lifetime_type (^^volatile void));
static_assert (is_implicit_lifetime_type (^^char));
static_assert (is_implicit_lifetime_type (^^signed char));
static_assert (is_implicit_lifetime_type (^^const unsigned char));
static_assert (is_implicit_lifetime_type (^^short));
static_assert (is_implicit_lifetime_type (^^volatile unsigned short));
static_assert (is_implicit_lifetime_type (^^int));
static_assert (is_implicit_lifetime_type (^^unsigned int));
static_assert (is_implicit_lifetime_type (^^const volatile long));
static_assert (is_implicit_lifetime_type (^^unsigned long));
static_assert (is_implicit_lifetime_type (^^long long));
static_assert (is_implicit_lifetime_type (^^unsigned long long));
#ifdef __SIZEOF_INT128__
__extension__ static_assert (is_implicit_lifetime_type (^^__int128));
__extension__ static_assert (is_implicit_lifetime_type (^^unsigned __int128));
#endif
static_assert (is_implicit_lifetime_type (^^float));
static_assert (is_implicit_lifetime_type (^^double));
static_assert (is_implicit_lifetime_type (^^long double volatile));
#ifdef __STDCPP_FLOAT16_T__
static_assert (is_implicit_lifetime_type (^^_Float16));
#endif
#ifdef __STDCPP_FLOAT32_T__
static_assert (is_implicit_lifetime_type (^^_Float32));
#endif
#ifdef __STDCPP_FLOAT64_T__
static_assert (is_implicit_lifetime_type (^^const _Float64));
#endif
#ifdef __STDCPP_FLOAT128_T__
static_assert (is_implicit_lifetime_type (^^_Float128));
#endif
#ifdef __STDCPP_BFLOAT16_T__
static_assert (is_implicit_lifetime_type (^^decltype(0.bf16)));
#endif
static_assert (is_implicit_lifetime_type (^^W));
static_assert (is_implicit_lifetime_type (^^const volatile X));
static_assert (is_implicit_lifetime_type (^^int *));
static_assert (is_implicit_lifetime_type (^^int (*) (int)));
static_assert (is_implicit_lifetime_type (^^int (Y::*)));
static_assert (is_implicit_lifetime_type (^^int (Y::*) (int)));
static_assert (!is_implicit_lifetime_type (^^int &));
static_assert (!is_implicit_lifetime_type (^^char &&));
static_assert (is_implicit_lifetime_type (^^int []));
__extension__ static_assert (!is_implicit_lifetime_type (^^int [0]));
static_assert (is_implicit_lifetime_type (^^int [1]));
static_assert (is_implicit_lifetime_type (^^const Y [42]));
static_assert (!is_implicit_lifetime_type (^^int ()));
static_assert (!is_implicit_lifetime_type (^^int () &));
static_assert (!is_implicit_lifetime_type (^^int () const));
static_assert (!is_implicit_lifetime_type (^^int (&) ()));
static_assert (is_implicit_lifetime_type (^^Z []));
static_assert (is_implicit_lifetime_type (^^Z [5]));
static_assert (is_implicit_lifetime_type (^^A));
static_assert (is_implicit_lifetime_type (^^B));
static_assert (is_implicit_lifetime_type (^^C));
static_assert (is_implicit_lifetime_type (^^D));
static_assert (is_implicit_lifetime_type (^^E));
static_assert (is_implicit_lifetime_type (^^F));
static_assert (is_implicit_lifetime_type (^^G));
static_assert (is_implicit_lifetime_type (^^H));
static_assert (is_implicit_lifetime_type (^^I));
static_assert (is_implicit_lifetime_type (^^J));
static_assert (!is_implicit_lifetime_type (^^K));
static_assert (!is_implicit_lifetime_type (^^L));
static_assert (is_implicit_lifetime_type (^^M));
static_assert (is_implicit_lifetime_type (^^N));
static_assert (is_implicit_lifetime_type (^^O));
static_assert (is_implicit_lifetime_type (^^P));
static_assert (!is_implicit_lifetime_type (^^Q));
static_assert (is_implicit_lifetime_type (^^R));
static_assert (!is_implicit_lifetime_type (^^S));
static_assert (is_implicit_lifetime_type (^^S [3]));
static_assert (is_implicit_lifetime_type (^^T));
static_assert (is_implicit_lifetime_type (^^U));
static_assert (is_implicit_lifetime_type (^^V));
static_assert (is_implicit_lifetime_type (^^_Complex double));
static_assert (is_implicit_lifetime_type (^^int [[gnu::vector_size (4 * sizeof (int))]]));
static_assert (is_implicit_lifetime_type (^^AA));
static_assert (is_implicit_lifetime_type (^^AB));
static_assert (!is_implicit_lifetime_type (^^AC));
static_assert (is_implicit_lifetime_type (^^AD));
static_assert (is_implicit_lifetime_type (^^AE));
static_assert (!is_implicit_lifetime_type (^^AF));

void
foo (int n)
{
  __extension__ static_assert (is_implicit_lifetime_type (^^char [n]));
}
