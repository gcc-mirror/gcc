// PR c++/67159
// { dg-do compile { target c++17_only } }
// { dg-options "-fconcepts" }
// { dg-additional-options "-fconcepts-diagnostics-depth=2" }

template <class T, class U>
concept bool SameAs = __is_same_as(T, U);

template <class T>
concept bool R1 = requires (T& t) { // { dg-message "in requirements" }
  { t.begin() } -> T;		// { dg-error "no match" }
  { t.end() } -> SameAs<T*>;	// { dg-message "does not satisfy" }
};

template <class T>
concept bool R2 = requires (T& t) { // { dg-message "in requirements" }
  { t.end() } -> SameAs<T*>;	// { dg-message "does not satisfy" }
};

struct foo {
  int* begin();
  int* end();
};

R1{T}
constexpr bool f() { return true; }

R2{T}
constexpr bool g() { return true; }

static_assert(f<foo>());	// { dg-error "" }
static_assert(g<foo>());	// { dg-error "" }
