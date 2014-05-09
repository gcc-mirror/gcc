// { dg-do compile { target c++11 } }

template <class T, class U> struct ST;
template <class T> struct ST<T,T> {};

int&& f();
const int&& g();

void h(bool b) {
  ST<decltype(b ? f() : g()),const int&&>();
}
