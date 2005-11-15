// PR c++/24687

extern "C" {
  template<typename _Tp>  // { dg-error "C" }   
  struct __is_pod {
    enum {
      __value = (sizeof(__gnu_internal::__test_type<_Tp>(0)))}; // { dg-error "declared|expected" }

// PR c++/24687

extern "C" {
  template<typename _Tp>  // { dg-error "C" }   
  struct __is_pod {
    enum {
      __value = (sizeof(__gnu_internal::__test_type<_Tp>(0)))}; // { dg-error "declared|expected" }

