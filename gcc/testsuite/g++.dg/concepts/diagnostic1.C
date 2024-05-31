// PR c++/67159
// { dg-do compile { target c++17 } }
// { dg-options "-fconcepts -fconcepts-diagnostics-depth=2" }

template <class T, class U>
concept SameAs = __is_same_as(T, U);

template <class T>
concept R1 = requires (T& t) {
  { t.begin() } -> T;		// { dg-error "no matching|return-type-requirement|too many" }
  { t.end() } -> SameAs<T*>;	// { dg-error "deduced expression type" }
};

template <class T>
concept R2 = requires (T& t) {
  { t.end() } -> SameAs<T*>;	// { dg-error "deduced expression type" }
};

struct foo {
  int* begin();
  int* end();
};

template<R1 T>
constexpr bool f() { return true; }

template<R2 T>
constexpr bool g() { return true; }

static_assert(f<foo>());	// { dg-error "" }
static_assert(g<foo>());	// { dg-error "" }
