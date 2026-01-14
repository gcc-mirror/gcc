// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::is_constructible_type.

#include <array>
#include <forward_list>
#include <list>
#include <meta>
#include <ranges>
#include <span>
#include <vector>

using namespace std::meta;

template <class... Args>
struct SupportArgs { 
  SupportArgs (Args...) noexcept;

  int operator() (Args...) noexcept;
};

void error();

template<typename Rg>
consteval bool
test_type_range(Rg&& rg)
{
  info _ = common_type(rg);
  info _ = common_reference(rg);
      
  if (!can_substitute (^^SupportArgs, rg)) error();
  auto target = substitute(^^SupportArgs, rg);
  if (!is_constructible_type (target, rg)) error();
  if (!is_nothrow_constructible_type (target, rg)) error();
  if (!!is_trivially_constructible_type (target, rg)) error();

  auto result = invoke_result(target, rg);
  if (!is_invocable_type (target, rg)) error();
  if (!is_invocable_r_type (result, target, rg)) error();
  if (!is_nothrow_invocable_type (target, rg)) error();
  if (!is_nothrow_invocable_r_type (result, target, rg)) error();
  return true;
}

constexpr info vt[] = {^^int, ^^int, ^^float, ^^int, ^^float, ^^double};
static_assert (test_type_range (vt | std::views::filter (is_integral_type))); // bidirectional
static_assert (test_type_range (vt | std::views::take_while (is_integral_type))); // non-common
static_assert (test_type_range (vt | std::views::to_input)); // input
static_assert (test_type_range (vt | std::views::cache_latest)); // input, move-only

template<typename Rg>
consteval bool
test_value_range(Rg&& rg)
{
  using V = std::ranges::range_value_t<Rg>;

  info ra = reflect_constant_array(rg);
  std::span<const V> va = define_static_array(rg);
  if (extract<const V*>(ra) != va.data())
    error();

  info rs = reflect_constant_string(rg);
  const V* vs = define_static_string(rg);
  if (extract<const V*>(rs) != vs)
    error();

  return true;
}

constexpr std::span<const char> vv = "abcd01234";
constexpr bool not_digit(char c) {
  return c < '0' || c > '9';
}

static_assert (test_value_range (vv | std::views::filter (not_digit))); // bidirectional
static_assert (test_value_range (vv | std::views::take_while (not_digit))); // non-common
static_assert (test_value_range (vv | std::views::to_input)); // input
static_assert (test_value_range (vv | std::views::cache_latest)); // input, move-only
								 
template<int> struct Aggr;
constexpr info dmt[] = {
  data_member_spec(^^int, {.name = "a"}),
  data_member_spec(^^int, {.name = "b"}),
  data_member_spec(^^float, {.name = "c"}),
  data_member_spec(^^double, {.name = "d"}),
};
consteval bool of_int_type(info dm) {
  return type_of(dm) == ^^int;
}

consteval {
  define_aggregate (^^Aggr<0>, dmt | std::views::filter (of_int_type)); // bidirectional
  define_aggregate (^^Aggr<1>, dmt | std::views::take_while (of_int_type)); // non-common
  define_aggregate (^^Aggr<2>, dmt | std::views::to_input); // input
  define_aggregate (^^Aggr<3>, dmt | std::views::cache_latest); // input, move-only
}
