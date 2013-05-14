// { dg-options "-std=c++1y" }

template <class T>
auto f() { return T::i; }

extern template auto f<int>(); // does not force instantiation
