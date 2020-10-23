// PR c++/96575
// { dg-do compile { target c++17 } }

struct S { };

constexpr auto g = [] (S s) {
  if (__builtin_is_constant_evaluated())
    return s;
};

template <class T>
constexpr auto f (T cb) {
  return [=] {
    auto ret = cb({});
    return ret;
  }();
}

constexpr auto x = f(g);
