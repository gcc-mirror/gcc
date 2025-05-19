// PR c++/120125
// { dg-additional-options "-fmodules -fdeclone-ctor-dtor" }
// { dg-module-cmi M }

export module M;

void foo();
export template <typename _Tp> struct __shared_ptr {
  inline __shared_ptr() { foo(); }
};

template class __shared_ptr<int>;
