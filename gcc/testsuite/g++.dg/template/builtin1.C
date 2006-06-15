// PR c++/26559

template<bool> struct cond;

template<int> struct S {
  void f(int i) {
    cond<__builtin_constant_p(i)>();
  }
};

S<1> s;
