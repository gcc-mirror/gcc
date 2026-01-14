// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::template_arguments_of.
// Taken from test/std/experimental/reflection/template-arguments.pass.cpp

#include <meta>

using namespace std::meta;

template <typename P1, auto P2, template <typename...> class P3>
struct TCls {
  template <typename> struct TInnerCls {};
};

template <typename P1, auto P2, template <typename...> class P3>
void TFn();

template <typename P1, auto P2, template <typename...> class P3>
int TVar = 0;

template <typename P1, auto P2, template <typename...> class P3>
using TAlias = int;

template <typename P1, auto P2, template <typename...> class P3>
concept Concept = requires { true; };


static_assert(template_arguments_of(^^TCls<int, 9, std::vector>).size() == 3);
static_assert(template_arguments_of(^^TCls<int, 9, std::vector>)[0] == ^^int);
static_assert([:template_arguments_of(^^TCls<int, 9, std::vector>)[1]:] == 9);
static_assert(template_arguments_of(^^TCls<int, 9, std::vector>)[2] == ^^std::vector);

static_assert(template_arguments_of(^^TFn<int, 9, std::vector>).size() == 3);
static_assert(template_arguments_of(^^TFn<int, 9, std::vector>)[0] == ^^int);
static_assert([:template_arguments_of(^^TFn<int, 9, std::vector>)[1]:] == 9);
static_assert(template_arguments_of(^^TFn<int, 9, std::vector>)[2] == ^^std::vector);

static_assert(template_arguments_of(^^TVar<int, 9, std::vector>).size() == 3);
static_assert(template_arguments_of(^^TVar<int, 9, std::vector>)[0] == ^^int);
static_assert([:template_arguments_of(^^TVar<int, 9, std::vector>)[1]:] == 9);
static_assert(template_arguments_of(^^TVar<int, 9, std::vector>)[2] == ^^std::vector);

static_assert(template_arguments_of(^^TAlias<int, 9, std::vector>).size() == 3);
static_assert(template_arguments_of(^^TAlias<int, 9, std::vector>)[0] == ^^int);
static_assert([:template_arguments_of(^^TAlias<int, 9, std::vector>)[1]:] == 9);
static_assert(template_arguments_of(^^TAlias<int, 9, std::vector>)[2] == ^^std::vector);

template <typename T>
using DependentAlias = TCls<int, 9, std::vector>::template TInnerCls<T>;
static_assert(template_arguments_of(^^DependentAlias<int>).size() == 1);
static_assert(template_arguments_of(^^DependentAlias<int>)[0] == ^^int);

template <typename T, T Val> struct WithDependentArgument {};
static_assert(template_arguments_of(^^WithDependentArgument<int, 5>).size() == 2);
static_assert(template_arguments_of(^^WithDependentArgument<int, 5>)[0] == ^^int);

template <typename... Ts> struct WithTypeParamPack {};
static_assert(template_arguments_of(^^WithTypeParamPack<int, bool>).size() == 2);
static_assert(template_arguments_of(^^WithTypeParamPack<int, bool>)[0] == ^^int);
static_assert(template_arguments_of(^^WithTypeParamPack<int, bool>)[1] == ^^bool);
static_assert(template_arguments_of(^^WithTypeParamPack<>).size() == 0);

struct S {
  int mem;
  bool operator==(const S&) const = default;
};
template <auto... Vs> struct WithAutoParamPack {};

static_assert(template_arguments_of(^^WithAutoParamPack<4, S{3}>).size() == 2);
static_assert([:template_arguments_of(^^WithAutoParamPack<4, S{3}>)[0]:] == 4);
static_assert([:template_arguments_of(^^WithAutoParamPack<4, S{3}>)[1]:] == S{3});
static_assert(template_arguments_of(^^WithAutoParamPack<>).size() == 0);

template <float> struct WithFloat {};
template <const float *> struct WithPtr {};
template <const int &> struct WithRef {};
template <info> struct WithReflection {};

constexpr float F = 4.5f;
static_assert(template_arguments_of(^^WithFloat<F>).size() == 1);
static_assert([:template_arguments_of(^^WithFloat<F>)[0]:] == F);

constexpr float Fs[] = {4.5, 5.5, 6.5};
static_assert(template_arguments_of(^^WithPtr<Fs>).size() == 1);
static_assert([:template_arguments_of(^^WithPtr<Fs>)[0]:] == +Fs);

static_assert(template_arguments_of(^^WithPtr<nullptr>).size() == 1);
static_assert([:template_arguments_of(^^WithPtr<nullptr>)[0]:] == nullptr);

const int I = 5;
using T = WithRef<I>;
static_assert(template_arguments_of(dealias(^^T)).size() == 1);
static_assert([:template_arguments_of(^^WithRef<I>)[0]:] == I);

static_assert(template_arguments_of(^^WithReflection<^^int>).size() == 1);
static_assert(template_arguments_of(^^WithReflection<^^int>)[0] == reflect_constant(^^int));

template <int &> void fn();
int p[2];
static_assert(template_arguments_of(^^fn<p[1]>)[0] == reflect_object(p[1]));

template<class T> struct X {};
X<int> obj1;
X<decltype(obj1)> obj2;

static_assert(has_template_arguments(template_arguments_of(^^decltype(obj2))[0])
	      == has_template_arguments(^^X<int>));
