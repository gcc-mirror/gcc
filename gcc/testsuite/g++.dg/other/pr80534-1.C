// PR c++/80534
// { dg-do compile { target c++11 } }
// { dg-options "" }

template <int> struct A {
  struct type {
    char __data[0];
  };
};
template <typename _Tp, typename = _Tp> struct B;
template <typename _Tp, typename _Dp> struct B<_Tp[], _Dp> {
  _Tp _M_t;
  using pointer = int;
  void m_fn1() {}
};
struct C {
  using Storage = A<0>::type;
  using StorageUniquePointer = B<Storage[]>;
  void m_fn2() { storageUniquePointer_.m_fn1(); }
  StorageUniquePointer storageUniquePointer_;
};
