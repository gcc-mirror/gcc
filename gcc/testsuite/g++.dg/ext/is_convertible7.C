// { dg-do compile { target c++11 } }

template <typename T, typename U>
struct is_convertible {
  static constexpr bool value = __is_convertible(T, U);
};

static_assert(is_convertible<int*, int>::value, "");  // { dg-error "assert" }
// { dg-error "invalid conversion" "" { target *-*-* } .-1 }

static_assert(is_convertible<int(), double (*)()>::value, "");  // { dg-error "assert" }
// { dg-error "invalid conversion" "" { target *-*-* } .-1 }

struct A {
  explicit A(int);
};
static_assert(is_convertible<int, A>::value, "");  // { dg-error "assert" }
// { dg-error "could not convert 'int' to 'A'" "" { target *-*-* } .-1 }

template <typename T, typename U>
struct is_nothrow_convertible {
  static constexpr bool value = __is_nothrow_convertible(T, U);
};

struct B {
  B(int);  // { dg-message "noexcept" }
};
static_assert(is_nothrow_convertible<int, B>::value, "");  // { dg-error "assert" }
// { dg-message "'int' is not nothrow convertible from 'B', because" "" { target *-*-* } .-1 }

struct C {
  explicit C(int);  // { dg-message "not considered" }
};
static_assert(is_convertible<int, C>::value, "");  // { dg-error "assert" }
// { dg-message "could not convert 'int' to 'C'" "" { target *-*-* } .-1 }

struct D {
  explicit operator int() const;  // { dg-message "not considered" }
};
static_assert(is_convertible<D, int>::value, "");  // { dg-error "assert" }
// { dg-message "could not convert 'D' to 'int'" "" { target *-*-* } .-1 }
