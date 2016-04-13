// PR c++/67159
// { dg-options "-std=c++1z -fconcepts" }

template <class T>
concept bool R = requires (T& t) {
  { t.begin() } -> T
};

struct foo {
  int* begin();
};

R{T}
constexpr bool f() { return true; }

static_assert(f<foo>());	// { dg-error "" }
