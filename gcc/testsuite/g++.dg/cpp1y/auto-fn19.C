// { dg-do compile { target c++14 } }

template <class T>
auto f() { return T::i; }

extern template auto f<int>(); // does not force instantiation
