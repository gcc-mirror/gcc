// { dg-do compile { target c++11 } }

template <typename T, typename... Args>
struct is_constructible {
  static constexpr bool value = __is_constructible(T, Args...);
};

static_assert(is_constructible<void>::value, "");  // { dg-error "assert" }
// { dg-message "'void' is not default constructible, because" "" { target *-*-* } .-1 }
// { dg-error "'void' is incomplete" "" { target *-*-* } .-2 }

static_assert(is_constructible<int&, const int&>::value, "");  // { dg-error "assert" }
// { dg-message "'int&' is not constructible from 'const int&', because" "" { target *-*-* } .-1 }
// { dg-error "discards qualifiers" "" { target *-*-* } .-2 }

static_assert(is_constructible<int, int, int>::value, "");  // { dg-error "assert" }
// { dg-message "'int' is not constructible from 'int, int', because" "" { target *-*-* } .-1 }
// { dg-error "too many initializers for non-class type 'int'" "" { target *-*-* } .-2 }

struct A {
  A(int);  // { dg-message "candidate" }
};
static_assert(is_constructible<A, int, int>::value, "");  // { dg-error "assert" }
// { dg-message "'A' is not constructible from 'int, int', because" "" { target *-*-* } .-1 }
// { dg-error "no matching function for call to" "" { target *-*-* } .-2 }

struct V {  // { dg-message "following virtual functions are pure" }
  virtual void foo() = 0;  // { dg-message "" }
};
static_assert(is_constructible<V>::value, "");  // { dg-error "assert" }
// { dg-error "object of abstract type" "" { target *-*-* } .-1 }

template <typename T, typename... Args>
struct is_nothrow_constructible {
  static constexpr bool value = __is_nothrow_constructible(T, Args...);
};

struct B {
  B(int);  // { dg-message "candidate" }
};
static_assert(is_nothrow_constructible<B>::value, "");  // { dg-error "assert" }
// { dg-message "'B' is not nothrow default constructible, because" "" { target *-*-* } .-1 }
// { dg-error "no matching function for call to" "" { target *-*-* } .-2 }

struct C {
  C(int);  // { dg-message "noexcept" }
};
static_assert(is_nothrow_constructible<C, int>::value, "");  // { dg-error "assert" }
// { dg-message "'C' is not nothrow constructible from 'int', because" "" { target *-*-* } .-1 }

template <typename T, typename... Args>
struct is_trivially_constructible {
  static constexpr bool value = __is_trivially_constructible(T, Args...);
};

struct D {
  D();  // { dg-message "non-trivial" }
};
static_assert(is_trivially_constructible<D>::value, "");  // { dg-error "assert" }
// { dg-message "'D' is not trivially default constructible, because" "" { target *-*-* } .-1 }

struct E {
  operator int();  // { dg-message "non-trivial" }
};
static_assert(is_trivially_constructible<int, E>::value, "");  // { dg-error "assert" }
// { dg-message "'int' is not trivially constructible from 'E', because" "" { target *-*-* } .-1 }
