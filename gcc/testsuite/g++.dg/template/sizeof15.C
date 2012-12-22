// PR c++/55685

typedef __SIZE_TYPE__ size_t;
template <size_t T, size_t U>
struct A;

template <typename T> struct B
{
  static A <sizeof (T), 0> x;
};

template <typename T>
A <sizeof (T), 0> B <T>::x;
