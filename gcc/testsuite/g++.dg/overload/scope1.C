// PR c++/9868

template <typename T> struct D {
  void (*p)();

  void clear() {
    D::p();
  }
};
template class D<bool>;
