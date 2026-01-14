// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::constant_of.

#include <meta>

using namespace std::meta;

[[=1, =1, =2, =1.0f]] int i;
static_assert (type_of (constant_of (annotations_of (^^i)[0])) == ^^int);
static_assert (type_of (constant_of (annotations_of (^^i)[1])) == ^^int);
static_assert (type_of (constant_of (annotations_of (^^i)[2])) == ^^int);
static_assert (type_of (constant_of (annotations_of (^^i)[3])) == ^^float);

[[=1, =1, =2, =1.0f]] void fn();
static_assert (type_of (constant_of (annotations_of (^^fn)[0])) == ^^int);
static_assert (type_of (constant_of (annotations_of (^^fn)[1])) == ^^int);
static_assert (type_of (constant_of (annotations_of (^^fn)[2])) == ^^int);
static_assert (type_of (constant_of (annotations_of (^^fn)[3])) == ^^float);

struct [[=3, =3, =4, =2.0f]] S;
static_assert (type_of (constant_of (annotations_of (^^S)[0])) == ^^int);
static_assert (type_of (constant_of (annotations_of (^^S)[1])) == ^^int);
static_assert (type_of (constant_of (annotations_of (^^S)[2])) == ^^int);
static_assert (type_of (constant_of (annotations_of (^^S)[3])) == ^^float);
#if 0
template <typename> struct [[=5, =5, =6, =3.0f]] TCls {};
// TODO Should work but we don't propagate the annotations correctly to the
// instantiation.  See also the TODO in annotations3.C.
static_assert (type_of (constant_of (annotations_of (^^TCls<int>)[0])) == ^^int);
static_assert (type_of (constant_of (annotations_of (^^TCls<int>)[1])) == ^^int);
static_assert (type_of (constant_of (annotations_of (^^TCls<int>)[2])) == ^^int);
static_assert (type_of (constant_of (annotations_of (^^TCls<int>)[3])) == ^^float);
#endif
template <typename> [[=5, =5, =6, =3.0f]] void TFn();
static_assert (type_of (constant_of (annotations_of (^^TFn<int>)[0])) == ^^int);
static_assert (type_of (constant_of (annotations_of (^^TFn<int>)[1])) == ^^int);
static_assert (type_of (constant_of (annotations_of (^^TFn<int>)[2])) == ^^int);
static_assert (type_of (constant_of (annotations_of (^^TFn<int>)[3])) == ^^float);
namespace [[=7, =7, =8, =4.0f]] NS {}
static_assert (type_of (constant_of (annotations_of (^^NS)[0])) == ^^int);
static_assert (type_of (constant_of (annotations_of (^^NS)[1])) == ^^int);
static_assert (type_of (constant_of (annotations_of (^^NS)[2])) == ^^int);
static_assert (type_of (constant_of (annotations_of (^^NS)[3])) == ^^float);

constexpr struct X {} x;
[[=x]] void foo ();

// TODO clang++ says ^^X, we say ^^const X.
static_assert (type_of (annotations_of (^^foo)[0]) == ^^const X);
static_assert (type_of (constant_of (annotations_of (^^foo)[0])) == ^^const X);

template<info R>
[[=[:constant_of (annotations_of (R)[0]):]]] void bar();

template<info R>
struct [[=[:constant_of (annotations_of (R)[0]):]]] Y {};

static_assert (extract<int>(annotations_of (^^bar<^^::fn>)[0]) == 1);
//static_assert (extract<int>(annotations_of (^^Y<^^::S>)[0]) == 3);

struct [[=42, =42.0f]] S1;
struct [[=42, =40]] S2;

static_assert (annotations_of (^^S1)[0] == annotations_of (^^S1)[0]);
static_assert (annotations_of (^^S1)[0] != annotations_of (^^S2)[0]);
static_assert (annotations_of (^^S1)[0] != annotations_of (^^S1)[1]);

static_assert (constant_of (annotations_of (^^S1)[0]) == reflect_constant (42));
static_assert (constant_of (annotations_of (^^S1)[0]) == constant_of (annotations_of (^^S2)[0]));

// A test from clang.
struct test_struct {};
constexpr test_struct test;

[[=test]] void func() {}
[[=1]] void func2() {}

constexpr auto func_first = constant_of (annotations_of (^^func)[0]);
constexpr auto func2_first = constant_of (annotations_of (^^func2)[0]);

static_assert (constant_of (^^test) == reflect_constant (test));
// TODO We evaluate the decltype to test_struct.
//static_assert (std::same_as<decltype([:func_first:]), const test_struct &>);
static_assert (func2_first == reflect_constant (1));
static_assert (func_first == reflect_constant (test));
