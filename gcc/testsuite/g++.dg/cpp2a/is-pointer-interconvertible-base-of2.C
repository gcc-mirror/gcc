// { dg-do compile { target c++20 } }
// { dg-additional-options "-Wno-inaccessible-base" }

template <typename T, typename U>
constexpr bool pointer_interconvertible_base_of
  = __is_pointer_interconvertible_base_of(T, U);

struct A {};
struct B {};  // { dg-message "not a pointer-interconvertible base" }
static_assert(pointer_interconvertible_base_of<A, B>);  // { dg-error "assert" }

struct C {
  int x;
private:
  int y;
};
struct D : C {};  // { dg-message "standard-layout" }
static_assert(pointer_interconvertible_base_of<C, D>);  // { dg-error "assert" }

struct E {};
struct F : E {};
struct G : F, E {};  // { dg-message "standard-layout" }
static_assert(pointer_interconvertible_base_of<E, G>);  // { dg-error "assert" }

union H {};  // { dg-message "non-union" }
static_assert(pointer_interconvertible_base_of<H, H>);  // { dg-error "assert" }

static_assert(pointer_interconvertible_base_of<A, int>);  // { dg-error "assert" }
// { dg-message "non-union" "" { target *-*-* } .-1 }
