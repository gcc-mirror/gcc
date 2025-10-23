// { dg-do compile { target c++17 } }
// Test diagnostics on static assertion failure.

template <typename T>
constexpr bool is_unique = __has_unique_object_representations(T);

struct AA {
  float x;  // { dg-message "float" }
};
static_assert(is_unique<AA>);  // { dg-error "assert" }

struct A : AA {};  // { dg-message "base" }
static_assert(is_unique<A>);  // { dg-error "assert" }

struct B {
  char   : 1;  // { dg-message "unnamed bit-field inserts padding before" }
  char x : 7;
};
static_assert(is_unique<B>);  // { dg-error "assert" }

struct C {
  char x : 5;  // { dg-message "padding occurs between 'C::x' and 'C::y'" }
  char y;
};
static_assert(is_unique<C>);  // { dg-error "assert" }

struct D {
  int x : 5;  // { dg-message "padding occurs" }
};
static_assert(is_unique<D>);  // { dg-error "assert" }

union E {
  char x[1];  // { dg-message "does not fill all bits" }
  char y[2];
};
static_assert(is_unique<E>);  // { dg-error "assert" }

union F {};  // { dg-message "no data fields" }
static_assert(is_unique<F>);  // { dg-error "assert" }

struct G { G(const G&); };  // { dg-message "trivially copyable" }
static_assert(is_unique<G>);  // { dg-error "assert" }

struct H {
  AA a;  // { dg-message "unique object representations" }
};
static_assert(is_unique<H>);  // { dg-error "assert" }
