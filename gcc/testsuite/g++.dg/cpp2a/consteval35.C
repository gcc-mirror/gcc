// { dg-do compile { target c++20 } }

template <typename T, typename F>
constexpr bool is_not(T t, F f) {
     return not f(t);
}

consteval bool is_even(int i) { return i % 2 == 0; }

static_assert(is_not(5, is_even)); // ok
