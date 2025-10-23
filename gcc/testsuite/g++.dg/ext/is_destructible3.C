// { dg-do compile { target c++11 } }

template <typename T>
struct is_destructible {
  static constexpr bool value = __is_destructible(T);
};

static_assert(is_destructible<void>::value, "");  // { dg-error "assert" }
// { dg-message "'void' is not destructible, because" "" { target *-*-* } .-1 }
// { dg-error "'void' is incomplete" "" { target *-*-* } .-2 }

static_assert(is_destructible<void() volatile>::value, "");  // { dg-error "assert" }
// { dg-message "'void\\(\\) volatile' is not destructible, because" "" { target *-*-* } .-1 }
// { dg-error "not a class or scalar type" "" { target *-*-* } .-2 }

struct A {
  ~A() = delete;  // { dg-message "declared here" }
};
static_assert(is_destructible<A>::value, "");  // { dg-error "assert" }
// { dg-message "'A' is not destructible, because" "" { target *-*-* } .-1 }
// { dg-error "use of deleted function" "" { target *-*-* } .-2 }

struct B {
private:
  ~B();  // { dg-message "declared private here" }
};
static_assert(is_destructible<B>::value, "");  // { dg-error "assert" }
// { dg-message "'B' is not destructible, because" "" { target *-*-* } .-1 }
// { dg-error "private within this context" "" { target *-*-* } .-2 }

template <typename T>
struct is_nothrow_destructible {
  static constexpr bool value = __is_nothrow_destructible(T);
};

struct C {
  ~C() noexcept(false);  // { dg-message "noexcept" }
};
static_assert(is_nothrow_destructible<C>::value, "");  // { dg-error "assert" }
// { dg-message "'C' is not nothrow destructible, because" "" { target *-*-* } .-1 }

struct D {
private:
  ~D() {}  // { dg-message "declared private here" }
};
static_assert(is_nothrow_destructible<D>::value, "");  // { dg-error "assert" }
// { dg-message "'D' is not nothrow destructible, because" "" { target *-*-* } .-1 }
// { dg-error "private within this context" "" { target *-*-* } .-2 }

template <typename T>
struct is_trivially_destructible {
  static constexpr bool value = __is_trivially_destructible(T);
};

struct E {
  ~E();
};
struct F { E d; };  // { dg-message "non-trivial" }
static_assert(is_trivially_destructible<F>::value, "");  // { dg-error "assert" }
// { dg-message "'F' is not trivially destructible, because" "" { target *-*-* } .-1 }

struct G {
private:
  ~G();  // { dg-message "declared private here" }
};
struct H { G g; };  // { dg-error "private within this context" }
static_assert(is_trivially_destructible<H>::value, "");  // { dg-error "assert" }
// { dg-message "'H' is not trivially destructible, because" "" { target *-*-* } .-1 }
// { dg-error "use of deleted function" "" { target *-*-* } .-2 }
