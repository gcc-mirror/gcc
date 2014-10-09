// PR c++/52892
// { dg-do compile { target c++11 } }

constexpr __SIZE_TYPE__ fibonacci(__SIZE_TYPE__ val) {
  return (val <= 2) ? 1 : fibonacci(val - 1) + fibonacci(val - 2);
}

template <typename Function>
struct Defer {
  constexpr Defer(const Function func_) : func(func_) { }

  const Function func;

  template <typename... Args>
  constexpr auto operator () (const Args&... args) -> decltype(func(args...)) {
    return func(args...);
  }
};

template <typename Function>
constexpr Defer<Function> make_deferred(const Function f) {
  return Defer<Function>(f);
}

int main() {
  constexpr auto deferred = make_deferred(&fibonacci);
  static_assert(deferred(25) == 75025, "Static fibonacci call failed"); // { dg-error "no match for call" "" { target c++14 } }
}
