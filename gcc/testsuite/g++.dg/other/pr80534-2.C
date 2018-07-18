// PR c++/80534
// { dg-do compile { target c++11 } }
// { dg-options "" }

template <int, int> struct aligned_storage {
  struct type {
    char __data[0];
  };
};
struct A {};
template <typename _Tp, typename = _Tp> struct unique_ptr;
template <typename _Tp, typename _Dp> struct unique_ptr<_Tp[], _Dp> {
  int _M_t;
  void get() { _M_t; }
};
struct B {
  using Association = A;
  using Storage = aligned_storage<sizeof(Association), alignof(Association)>::type;
  using StorageUniquePointer = unique_ptr<Storage[]>;
  void getAssociationsBegin() { storageUniquePointer_.get(); }
  StorageUniquePointer storageUniquePointer_;
};
struct C {};
using MainThreadStaticSignalsReceiver = C;
aligned_storage<sizeof(MainThreadStaticSignalsReceiver),
                alignof(MainThreadStaticSignalsReceiver)>::type
    mainThreadStaticSignalsReceiverStorage;
