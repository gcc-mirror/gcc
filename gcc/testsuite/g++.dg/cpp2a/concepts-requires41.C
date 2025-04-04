// PR c++/117849
// { dg-do compile { target c++20 } }

template<int N>
struct array {
  constexpr int size() const { return N; }
};

struct vector {
  int _size = 3;
  constexpr int size() const { return _size; }
};

template<int N>
struct integral_constant {
  constexpr operator int() const { return N; }
};

template<class T>
concept StaticSize = requires (T& t) {
  typename integral_constant<t.size()>;
};

static_assert(StaticSize<array<5>>);
static_assert(!StaticSize<vector>);
