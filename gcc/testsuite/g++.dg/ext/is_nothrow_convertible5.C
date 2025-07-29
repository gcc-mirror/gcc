// PR c++/121291
// { dg-do compile { target c++17 } }

template <typename T, typename U>
constexpr bool is_nothrow_convertible = __is_nothrow_convertible(T, U);

struct A {};
struct B {
private:
  operator A() noexcept;  // { dg-message "here" }
};

static_assert(is_nothrow_convertible<B, A>);  // { dg-error "assert" }
// { dg-message "not nothrow convertible" "" { target *-*-* } .-1 }
// { dg-error "private within this context" "" { target *-*-* } .-2 }
