// PR c++/67159
// { dg-options "-std=c++17 -fconcepts" }

template <class T, class U>
concept bool SameAs = __is_same_as(T, U);

template <class T>
concept bool R1 = requires (T& t) {
  { t.begin() } -> T
  { t.end() } -> SameAs<T*>;
};

template <class T>
concept bool R2 = requires (T& t) {
  { t.end() } -> SameAs<T*>;
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
