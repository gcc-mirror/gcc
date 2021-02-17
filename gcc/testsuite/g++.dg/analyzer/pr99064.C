// { dg-do compile { target c++17 } }
// { dg-additional-options "-Wno-analyzer-too-complex" } */

template <typename> struct iterator_traits;
template <typename _Tp> struct iterator_traits<_Tp *> {
  typedef _Tp &reference;
};
template <typename _Iterator> struct __normal_iterator {
  _Iterator _M_current;
  __normal_iterator(_Iterator &__i) : _M_current(__i) {}
  typename iterator_traits<_Iterator>::reference operator*() {
    return *_M_current;
  }
};
template <typename> struct allocator;
template <typename> struct allocator_traits;
template <typename _Tp> struct allocator_traits<allocator<_Tp>> {
  using pointer = _Tp *;
};
struct TPkcs11Token;
struct __alloc_traits : allocator_traits<allocator<TPkcs11Token>> {};
struct _Vector_base {
  typedef __alloc_traits::pointer pointer;
  struct {
    pointer _M_start;
  } _M_impl;
};
struct : _Vector_base {
  __normal_iterator<pointer> begin() { return _M_impl._M_start; }
} list_tokens_token_list;
struct TPkcs11Token {
  int *add_info;
};
void list_tokens() {
  for (__normal_iterator base = list_tokens_token_list.begin();;) {
    int *add_info = new int;
    (*base).add_info = add_info; // { dg-warning "leak" }
  }
}
