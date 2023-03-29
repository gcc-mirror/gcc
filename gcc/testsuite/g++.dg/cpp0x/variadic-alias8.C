// PR c++/101071
// { dg-do compile { target c++11 } }
// { dg-additional-options "-fno-elide-constructors -O2" }

// Like variadic-alias2.C, just different options.

template<class...>
struct list {};

struct nil;

////////////////////////////////////////////////////////////////////////////////

template<int n>
struct number {
  constexpr /*implicit*/ operator int() const { return n; }
  using type = number<n>;
};

using false_ = number<0>;
using true_ = number<1>;

static_assert(!false_{}, "");
static_assert(true_{}, "");

template<int... ns> using numbers = list<number<ns>...>;

////////////////////////////////////////////////////////////////////////////////

template<class lhs, class rhs>
struct less_impl;

template<int lhs, int rhs>
struct less_impl<number<lhs>, number<rhs>>
  : number<(lhs < rhs)> {};

template<class lhs, class rhs> using less = typename less_impl<lhs, rhs>::type;

////////////////////////////////////////////////////////////////////////////////

template<class v0, class... vs>
struct sum_impl {
  static_assert(sizeof...(vs) == 0, "see specialization");
  using type = v0;
};

template<int v0, int v1, class... vs>
struct sum_impl<number<v0>, number<v1>, vs...>
  : sum_impl<number<v0 + v1>, vs...> {};

template<class... nums> using sum = typename sum_impl<nums...>::type;

////////////////////////////////////////////////////////////////////////////////

template<class num>
struct conditional_impl {
  static_assert(num{}, "see specialization");

  template<class T, class F>
  using type = T;
};

template<>
struct conditional_impl<false_> {
  template<class T, class F>
  using type = F;
};

template<class num, class T, class F>
using conditional = typename conditional_impl<num>::template type<T, F>;

////////////////////////////////////////////////////////////////////////////////

template<class seq>
struct min_filter_impl;

template<class... nums>
struct min_filter_impl<list<nums...>> {
  template<class num>
  using count_better_mins = sum<less<nums, num>...>;

  using type = list<conditional<count_better_mins<nums>, nil, nums>...>;
};

template<class seq> using min_filter = typename min_filter_impl<seq>::type;

////////////////////////////////////////////////////////////////////////////////

void test_min_filter() {
  using computed = min_filter<numbers<2, 7, 2>>;
  using expected = list<number<2>, nil, number<2>>;
  (void)(computed{} = expected{});// compiles for identical types
}

int main() {}
