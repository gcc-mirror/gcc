#include <stdlib.h>
#include <string>
#include <vector>

template <class T> struct Ptr {
  void Store(T *ptr) { t = ptr; }

  void Access() { *t = {}; }

  T *t;
};

template <class T, size_t N> struct Ptr<T[N]> {
  using Type = T[N];
  void Store(Type *ptr) { t = *ptr; }

  void Access() { *t = {}; }

  T *t;
};

template <class T> __attribute__((noinline)) void test() {
  Ptr<T> ptr;
  {
    T x;
    ptr.Store(&x);
  }

  ptr.Access();
}
