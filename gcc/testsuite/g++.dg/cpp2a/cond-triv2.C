// PR c++/67348
// { dg-do compile { target c++20 } }

#include <type_traits>
#include <vector>
using namespace std;

template <class...Ts>
  requires (is_destructible<Ts>::value && ...)
struct variant {
  ~variant() { /* ... */ }
  ~variant()
    requires (is_trivially_destructible<Ts>::value && ...) = default;

  variant(variant&&) { /* ... */ }
  variant(variant&&)
    requires (is_trivially_move_constructible<Ts>::value && ...) = default;

  variant& operator=(variant&&) { /* ... */ }
  variant& operator=(variant&&)
    requires (is_trivially_move_assignable<Ts>::value && ...) = default;

  // ...similar treatment for copy construction / assignment...
};

static_assert(is_trivially_destructible<variant<int, float>>());
static_assert(!is_trivially_destructible<variant<int, vector<int>>>());

static_assert(is_trivially_move_constructible<variant<int, float>>());
static_assert(!is_trivially_move_constructible<variant<int, vector<int>>>());
