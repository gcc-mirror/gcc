// PR c++/121291
// { dg-do compile { target c++17 } }

template <typename T>
constexpr bool is_invocable = __is_invocable(T);

template <typename T>
constexpr bool is_nothrow_invocable = __is_nothrow_invocable(T);

struct S {
private:
  int operator()() noexcept;  // { dg-message "here" }
};

static_assert(is_invocable<S>);  // { dg-error "assert" }
// { dg-message "not invocable" "" { target *-*-* } .-1 }
// { dg-error "private within this context" "" { target *-*-* } .-2 }

static_assert(is_nothrow_invocable<S>);  // { dg-error "assert" }
// { dg-message "not nothrow invocable" "" { target *-*-* } .-1 }
// { dg-error "private within this context" "" { target *-*-* } .-2 }
