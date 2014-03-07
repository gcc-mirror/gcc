// { dg-do compile { target c++1y } }

template <class T>
auto f() { return T::i; }

extern template auto f<int>(); // does not force instantiation
