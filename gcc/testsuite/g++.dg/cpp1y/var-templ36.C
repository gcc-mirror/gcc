// { dg-do compile { target c++14 } }

template <class T>
constexpr T v = T();

template <class T>
constexpr T v<T*> = T();

template <class T>
struct A {
  static constexpr decltype (v<T>) v = ::v<T>;
};

double d1 = v<double*>;
double d2 = A<double*>::v;
