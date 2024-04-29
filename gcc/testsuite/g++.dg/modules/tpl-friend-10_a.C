// PR c++/105320
// { dg-additional-options "-fmodules-ts -Wno-global-module" }
// { dg-module-cmi test_support }

module;
template<class> struct _Sp_atomic;
template<class> struct shared_ptr {
  template<class> friend struct _Sp_atomic;
  using atomic_type = _Sp_atomic<int>;
};
export module test_support;
export
template<class T> struct A {
   shared_ptr<T> data;
};
