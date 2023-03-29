// PR c++/107295
// { dg-do compile { target c++11 } }

template <typename T> struct A {
  typedef T __attribute__((vector_size (sizeof (int)))) V;
};
template <int, typename T> using B = typename A<T>::V;
template <typename T> using V = B<4, T>;
using F = V<float>;
constexpr F a = F () + 0.0f;
constexpr F b = F () + (float) 0.0;
constexpr F c = F () + (float) 0.0L;
