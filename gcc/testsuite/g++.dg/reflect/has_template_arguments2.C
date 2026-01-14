// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::has_template_arguments.

#include <meta>
#include <vector>

using namespace std::meta;

template <typename P1, auto P2, template <typename...> class P3>
struct class_tmpl {
  template <typename> struct inner {};
};
static_assert (!has_template_arguments (^^class_tmpl));
static_assert (has_template_arguments (^^class_tmpl<int, 9, std::vector>));

template <typename P1, auto P2, template <typename...> class P3>
void fn_tmpl ();
static_assert (!has_template_arguments (^^fn_tmpl));
static_assert (has_template_arguments (^^fn_tmpl<int, 9, std::vector>));

template <typename P1, auto P2, template <typename...> class P3>
int var_tmpl = 0;
static_assert (!has_template_arguments (^^var_tmpl));
static_assert (has_template_arguments (^^var_tmpl<int, 9, std::vector>));

template <typename P1, auto P2, template <typename...> class P3>
using alias = int;
static_assert (!has_template_arguments (^^alias));
static_assert (has_template_arguments (^^alias<int, 9, std::vector>));

template <typename T>
using dep_alias = class_tmpl<int, 9, std::vector>::template inner<T>;
static_assert (!has_template_arguments (^^dep_alias));
static_assert (has_template_arguments (^^dep_alias<int>));

struct Der : class_tmpl<int, 9, std::vector> {};
static_assert (!has_template_arguments (^^Der));

template <typename T, T> struct A {};
static_assert (!has_template_arguments (^^A));
static_assert (has_template_arguments (^^A<int, 5>));

template <typename... Ts> struct B {};
static_assert (!has_template_arguments (^^B));
static_assert (has_template_arguments (^^B<int, bool>));

struct S {
  int mem;
  bool operator==(const S&) const = default;
};
template <auto... Vs> struct auto_pack {};
static_assert (!has_template_arguments (^^auto_pack));
static_assert (has_template_arguments (^^auto_pack<4, S{3}>));

template <float> struct float_targ {};
constexpr float F = 4.5f;
static_assert (!has_template_arguments (^^float_targ));
static_assert (has_template_arguments (^^float_targ<F>));

template <const float *> struct ptr_targ {};
constexpr float Fs[] = {4.5, 5.5, 6.5};
static_assert (!has_template_arguments (^^ptr_targ));
static_assert (has_template_arguments (^^ptr_targ<Fs>));
static_assert (has_template_arguments (^^ptr_targ<nullptr>));

template <const int &> struct ref_targ {};
const int I = 5;
using T = ref_targ<I>;
static_assert (!has_template_arguments (^^ref_targ));
static_assert (has_template_arguments (dealias(^^T)));

template <std::meta::info> struct refl_targ {};
static_assert (!has_template_arguments (^^refl_targ));
static_assert (has_template_arguments (^^refl_targ<^^int>));
