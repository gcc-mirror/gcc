// PR c++/119688
// { dg-do compile { target c++20 } }

template<int N>
struct builder {
  bool value[256]{};
  constexpr builder(char const (&s)[N]) {
    for(int i = 0 ; i < N; ++i)
      value[static_cast<unsigned char>(s[i])] = true;
  }
};

template<builder A>
constexpr auto operator""_ar() {
  return A.value;
}

constexpr auto first = "ab"_ar;
static_assert( first['a']);
static_assert( first['b']);
static_assert(!first['c']);
static_assert(!first['d']);
static_assert(!first['z']);

constexpr auto second = "cd"_ar;
static_assert(!second['a']);
static_assert(!second['b']);
static_assert(!second['z']);
static_assert( second['c']);
static_assert( second['d']);
