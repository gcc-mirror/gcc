// { dg-do compile { target c++11 } }

template <typename T>
struct is_copy_assignable {
  static constexpr bool value = __is_assignable(T&, const T&);
};

static_assert(is_copy_assignable<const int>::value, "");  // { dg-error "assert" }
// { dg-error "assignment to read-only type 'const int'" "" { target *-*-* } .-1 }

struct A {
  void operator=(A) = delete;  // { dg-message "declared here" }
};
static_assert(is_copy_assignable<A>::value, "");  // { dg-error "assert" }
// { dg-message "is not assignable" "" { target *-*-* } .-1 }
// { dg-error "use of deleted function" "" { target *-*-* } .-2 }

template <typename T>
struct is_nothrow_copy_assignable {
  static constexpr bool value = __is_nothrow_assignable(T&, const T&);
};
struct B {
  void operator=(const B&);  // { dg-message "noexcept" }
};
static_assert(is_nothrow_copy_assignable<B>::value, "");  // { dg-error "assert" }
// { dg-message "is not nothrow assignable" "" { target *-*-* } .-1 }

template <typename T>
struct is_trivially_copy_assignable {
  static constexpr bool value = __is_trivially_assignable(T&, const T&);
};
struct C {
  void operator=(const C&);  // { dg-message "non-trivial" }
};
static_assert(is_trivially_copy_assignable<C>::value, "");  // { dg-error "assert" }
// { dg-message "is not trivially assignable" "" { target *-*-* } .-1 }
