// PR c++/106214
// { dg-do compile { target c++17 } }
// A version of cpp0x/initlist-deduce3.C using list CTAD instead
// of ordinary auto deduction from an initializer list.

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

template<class T>
struct vector {
  vector(std::initializer_list<T>);
};

vector a = &Task<int>; // { dg-error "deduction|no match" }
vector b = { &Task<int> };
vector e{ &Task<int> };
vector f = { &Task<int>, &Task<int> };
vector d = { static_cast<void(*)()>(&Task<int>) };
