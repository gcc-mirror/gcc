// PR c++/55842
// { dg-options -std=c++11 }

template <class=void> struct number {
  number() noexcept(noexcept(0)) { }
};
const int z=__has_nothrow_constructor(number<>);
