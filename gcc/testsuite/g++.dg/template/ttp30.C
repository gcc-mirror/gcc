// PR c++/78898

struct A {
  template <class T> A(T);
  template <template <typename> class SmartPtr> A(SmartPtr<int>) { A(0); }
};
