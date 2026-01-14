// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

using info = decltype(^^int);

namespace colon_parsing {
constexpr int x = 4;
constexpr auto rx = ^^x;
static_assert([:rx:] == 4);

constexpr unsigned Idx = 1;
constexpr int arr[] = {1, 2, 3};
static_assert(arr[::colon_parsing::Idx] == 2);

constexpr info rIdx = ^^Idx;
static_assert([:::colon_parsing::rIdx:] == 1);

struct WithIndexOperator {
  bool operator[:>(int);  // Test interaction with ':>'-digraph (i.e., ']').
};
}
