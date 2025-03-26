// { dg-additional-options "-fmodule-header" }
// { dg-module-cmi {} }

template <typename T> struct __shared_ptr {
  template <typename> friend struct out_ptr_t;
};
inline namespace __cxx11 __attribute__((__abi_tag__)) {
  struct _Impl;
}
struct path {
  __shared_ptr<_Impl> _M_impl;
};
