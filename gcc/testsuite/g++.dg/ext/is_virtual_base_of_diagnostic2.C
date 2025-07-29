// { dg-do compile { target c++11 } }

template <typename T, typename U>
struct is_virtual_base_of {
  static constexpr bool value = __builtin_is_virtual_base_of(T, U);
};

static_assert(is_virtual_base_of<int, int>::value, "");  // { dg-error "assert" }
// { dg-message "'int' is not a virtual base of 'int'" "" { target *-*-* } .-1 }

struct A {};  // { dg-message "'A' is not a virtual base of 'B'" }
struct B : A {};  // { dg-message "declared here" }
static_assert(is_virtual_base_of<A, B>::value, "");  // { dg-error "assert" }
