// PR c++/120555
// { dg-do compile { target c++17 } }

struct A { int m; };

template<class T>
constexpr auto f() {
  if constexpr (sizeof(T) == sizeof(int))
    return 1;
  else
    return A{f<int>()};
}

static_assert(f<bool>().m == 1);
static_assert(f<int>() == 1);

template <class T> constexpr auto g();

template<class T>
constexpr auto f2() {
  if constexpr (sizeof(T) == sizeof(int))
    return 1;
  else
    return A{g<int>()};		// { dg-error "auto" }
}

template <class T> constexpr auto g() { return A{1}; }

static_assert(f2<bool>().m == 1);
static_assert(f2<int>() == 1);
