// PR c++/14032

template <typename T> struct outer {
  template <typename T2, typename U>
  struct inner {
    static int f() { return inner<T,int>::N; };
  };

  template <typename U>
  struct inner<T,U> {
    static const int N = 1;
  };
};

int i = outer<int>::inner<double,int>::f();
