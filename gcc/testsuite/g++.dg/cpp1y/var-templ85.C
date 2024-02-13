// PR c++/113612
// { dg-do compile { target c++14 } }

template <typename T> T t;
template <typename T> extern T *t<T *>;
template <typename T> T *t<T *> = t<int>;
