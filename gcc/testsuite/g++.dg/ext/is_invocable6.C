// { dg-do compile { target c++11 } }

template <typename F, typename... Args>
struct is_invocable {
  static constexpr bool value = __is_invocable(F, Args...);
};

static_assert(is_invocable<int>::value, "");  // { dg-error "assert" }
// { dg-message "'int' is not invocable, because" "" { target *-*-* } .-1 }
// { dg-error "'int' cannot be used as a function" "" { target *-*-* } .-2 }

static_assert(is_invocable<void(*)(), int>::value, "");  // { dg-error "assert" }
// { dg-message "'void \[^'\]*' is not invocable by 'int', because" "" { target *-*-* } .-1 }
// { dg-error "too many arguments" "" { target *-*-* } .-2 }

static_assert(is_invocable<void(void*), void() const>::value, "");  // { dg-error "assert" }
// { dg-message "'void.void..' is not invocable by 'void.. const', because" "" { target *-*-* } .-1 }
// { dg-error "qualified function type" "" { target *-*-* } .-2 }

struct A {};
static_assert(is_invocable<const A&&, int, double>::value, "");  // { dg-error "assert" }
// { dg-message "'const A&&' is not invocable by 'int, double', because" "" { target *-*-* } .-1 }
// { dg-error "no match for call to " "" { target *-*-* } .-2 }

struct B {
  void operator()() = delete;  // { dg-message "declared here" }
};
static_assert(is_invocable<B>::value, "");  // { dg-error "assert" }
// { dg-message "'B' is not invocable, because" "" { target *-*-* } .-1 }
// { dg-error "use of deleted function" "" { target *-*-* } .-2 }

template <typename F, typename... Args>
struct is_nothrow_invocable {
  static constexpr bool value = __is_nothrow_invocable(F, Args...);
};

static_assert(is_nothrow_invocable<void(*)()>::value, "");  // { dg-error "assert" }
// { dg-message "'void \[^'\]*' is not nothrow invocable, because" "" { target *-*-* } .-1 }
// { dg-message "'void \[^'\]*' is not 'noexcept'" "" { target *-*-* } .-2 }

struct C {
  int operator()(int, double) const;  // { dg-message "noexcept" }
};
static_assert(is_nothrow_invocable<const C&, int, int>::value, "");  // { dg-error "assert" }
// { dg-message "'const C&' is not nothrow invocable by 'int, int', because" "" { target *-*-* } .-1 }
