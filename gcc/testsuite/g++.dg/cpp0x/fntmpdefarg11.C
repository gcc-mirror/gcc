// PR c++/87480
// { dg-do compile { target c++11 } }

template<typename T> T&& declval();

template <typename T, typename = decltype(declval<T>().d)> void f(T) { }

struct A {
  double d;
};

template <typename>
void j(A& a) {
  f(a);
}
