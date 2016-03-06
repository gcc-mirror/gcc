// PR c++/67364
// { dg-do compile { target c++11 } }

template <typename Xn>
struct tuple {
  Xn storage_;

  constexpr tuple(Xn const& xn)
    : storage_(xn)
  { }

  template <typename ...dummy>
  constexpr tuple(tuple const& other)
    : storage_(other.storage_)
  { }

  template <typename ...dummy>
  constexpr tuple(tuple& other)
    : tuple(const_cast<tuple const&>(other))
  { }
};

template <typename T>
struct wrapper { T value; };

template <typename T>
constexpr wrapper<T> wrap(T t) { return {t}; }

constexpr wrapper<tuple<int>> t = wrap(tuple<int>{2});
static_assert(t.value.storage_ == 2, "");
