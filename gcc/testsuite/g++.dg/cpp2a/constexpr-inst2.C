// { dg-do compile { target c++11 } }

struct __as_receiver {
  int empty_env;
};

template<class T>
constexpr int f(T t) {
  return t.fail;
};

using type = decltype(__as_receiver{f(0)}); // OK, f<int> not instantiated
