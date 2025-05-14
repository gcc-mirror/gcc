// PR c++/118928
// { dg-do compile { target c++11 } }
// { dg-options "-O" }

using size_t = decltype(sizeof(0));

namespace std {
template <typename T> struct initializer_list {
  const T *_M_array;
  size_t _M_len;
};
struct S {
  constexpr S(const char *); // { dg-warning "used but never defined" }
};
struct vector {
  constexpr vector(initializer_list<S>) {}
};
}
struct Y {
    std::vector v;
};
struct X {
  Y y{{""}};
} x;
