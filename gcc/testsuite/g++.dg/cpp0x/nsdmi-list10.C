// PR c++/118047
// { dg-do compile { target c++11 } }

typedef decltype(sizeof(char)) size_t;

namespace std {
template <typename T>
struct initializer_list {
  const T *_M_array;
  size_t _M_len;
  constexpr size_t size() const { return _M_len; }
};
}

enum E {
    One
};

struct A {
    E e = One;
};

struct B {
    A as[1] {};
};

struct V
{
  constexpr V(const std::initializer_list<B> &a) : size(a.size()){}
  int size;
};

constexpr V a{{}};

static_assert(a.size == 1, "");
