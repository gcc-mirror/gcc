// { dg-do assemble  }

template <class T>
struct S {
  typedef T X;

  class C {
    typedef T X;
  };
};

template <int I>
struct S2 {
  enum { A = I };

  void f() {
    int A;
  }
};
