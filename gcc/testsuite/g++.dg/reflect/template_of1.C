// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::template_of.

#include <meta>
#include <vector>

using namespace std::meta;

template<typename> struct incomplete_cls;
template<typename> struct cls_tmpl {
  template <typename T> struct inner {};
};
template<typename> void fun_tmpl ();
template<typename> int var_tmpl;
template<typename T> using cls_tmpl_alias = cls_tmpl<T>;
template<typename> using int_alias = int;
template<typename U> using dep_alias = cls_tmpl<int>::template inner<U>;

static_assert (template_of (^^cls_tmpl<int>) == ^^cls_tmpl);
static_assert (template_of (^^incomplete_cls<int>) == ^^incomplete_cls);
static_assert (template_of (^^int_alias<int>) == ^^int_alias);
static_assert (template_of (^^dep_alias<bool>) == ^^dep_alias);
static_assert (template_of (^^fun_tmpl<int>) == ^^fun_tmpl);
static_assert (template_of (^^var_tmpl<int>) == ^^var_tmpl);
static_assert (template_of (^^cls_tmpl_alias<int>) == ^^cls_tmpl_alias);

struct W {
  using U = cls_tmpl<int>;
};
static_assert (template_of (dealias (^^W::U)) == ^^cls_tmpl);

template <typename P1, auto P2, template <typename...> class P3>
struct class_tmpl {
  template <typename> struct inner {};
};
static_assert (template_of (^^class_tmpl<int, 9, std::vector>) == ^^class_tmpl);

template <typename P1, auto P2, template <typename...> class P3>
void fn_tmpl ();
static_assert (template_of (^^fn_tmpl<int, 9, std::vector>) == ^^fn_tmpl);

template <typename P1, auto P2, template <typename...> class P3>
int var_tmpl2 = 0;
static_assert (template_of (^^var_tmpl2<int, 9, std::vector>) == ^^var_tmpl2);

template <typename P1, auto P2, template <typename...> class P3>
using alias = int;
static_assert (template_of (^^alias<int, 9, std::vector>) == ^^alias);

template <typename T>
using dep_alias2 = class_tmpl<int, 9, std::vector>::template inner<T>;
static_assert (template_of (^^dep_alias2<int>) == ^^dep_alias2);

template <typename T, T> struct A {};
static_assert (template_of (^^A<int, 5>) == ^^A);

template <typename... Ts> struct B {};
static_assert (template_of (^^B<int, bool>) == ^^B);

struct S {
  int mem;
  bool operator==(const S&) const = default;
};
template <auto... Vs> struct auto_pack {};
static_assert (template_of (^^auto_pack<4, S{3}>) == ^^auto_pack);

template <float> struct float_targ {};
constexpr float F = 4.5f;
static_assert (template_of (^^float_targ<F>) == ^^float_targ);

template <const float *> struct ptr_targ {};
constexpr float Fs[] = {4.5, 5.5, 6.5};
static_assert (template_of (^^ptr_targ<Fs>) == ^^ptr_targ);
static_assert (template_of (^^ptr_targ<nullptr>) == ^^ptr_targ);

template <const int &> struct ref_targ {};
const int I = 5;
using T = ref_targ<I>;
static_assert (template_of (dealias(^^T)) == ^^ref_targ);

template <std::meta::info> struct refl_targ {};
static_assert (template_of (^^refl_targ<^^int>) == ^^refl_targ);

struct M {
  template<typename T>
  struct N { };
};

template<typename T>
struct TS {
  template<typename U>
  struct N { };
};

static_assert (template_of (^^M::N<int>) == ^^M::N);
static_assert (template_of (^^TS<int>::N<int>) == ^^TS<int>::N);

template<typename T>
struct X { };

template<typename T>
struct X<T*> { };

template<>
struct X<int> { };

static_assert (template_of (^^X<int>) == ^^X);
static_assert (template_of (^^X<int *>) == ^^X);

template<typename = int>
struct Y { };
static_assert (template_of (^^Y<>) == ^^Y);

struct Z {
  using Alias = X<int>;
};

static_assert (template_of (dealias (^^Z::Alias)) == ^^X);
