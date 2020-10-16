// PR c++/93107
// { dg-do compile { target c++11 } }

using size_t = decltype(sizeof 0);

namespace std {
  template<typename T> struct initializer_list {
    const T *ptr;
    size_t n;
    initializer_list(const T*, size_t);
  };
}

template<typename T>
void Task() {}

auto a = &Task<int>;
auto b = { &Task<int> };
auto e{ &Task<int> };
auto f = { &Task<int>, &Task<int> };
std::initializer_list<void(*)()> c = { &Task<int> };
auto d = { static_cast<void(*)()>(&Task<int>) };
