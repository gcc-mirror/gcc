// { dg-additional-options -fmodules }

export module M;

[[deprecated]] void depr_fn() {}

#pragma GCC diagnostic ignored "-Wdeprecated-declarations"

export {
  template <class T> void f(T) { depr_fn(); }
}
