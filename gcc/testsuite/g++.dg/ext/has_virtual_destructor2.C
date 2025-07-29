// { dg-do compile { target c++11 } }

template <typename T> struct has_virtual_destructor {
  static constexpr bool value = __has_virtual_destructor(T);
};

static_assert(has_virtual_destructor<int>::value, "");  // { dg-error "assert" }
// { dg-message "'int' does not have a virtual destructor" "" { target *-*-* } .-1 }

struct A {};  // { dg-message "'A' does not have a virtual destructor" }
static_assert(has_virtual_destructor<A>::value, "");  // { dg-error "assert" }

struct B {
  ~B();  // { dg-message "'B' does not have a virtual destructor" }
};
static_assert(has_virtual_destructor<B>::value, "");  // { dg-error "assert" }

struct C {  // { dg-bogus "" }
  virtual ~C();  // { dg-bogus "" }
};
static_assert(has_virtual_destructor<C[5]>::value, "");  // { dg-error "assert" }
// { dg-message "'C \\\[5\\\]' does not have a virtual destructor" "" { target *-*-* } .-1 }

union U {  // { dg-message "'U' does not have a virtual destructor" }
  ~U();
};
static_assert(has_virtual_destructor<U>::value, "");  // { dg-error "assert" }
