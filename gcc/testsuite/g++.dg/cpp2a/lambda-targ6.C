// PR c++/88313
// { dg-do compile { target c++20 } }

template<int N = [](auto x) { return x; }(42)>
constexpr int f() { return N; }

template<auto F = [](auto x) { return x; }>
constexpr auto g() { return F; }

template<class T = decltype([](auto x) { return x; })>
constexpr int h(T t = {}) { return t(42); }

static_assert(f() == 42);
static_assert(g()(42) == 42);
static_assert(h() == 42);
