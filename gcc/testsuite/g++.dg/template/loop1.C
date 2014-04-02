// PR c++/60379

template <int> struct A {
  void m_fn1(int p1) {
    int *a;
    while (p1 && *static_cast<int *>(static_cast<void *>(a)))
      ;
  }
};
