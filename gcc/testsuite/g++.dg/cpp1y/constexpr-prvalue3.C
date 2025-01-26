// PR c++/118523
// { dg-do compile { target c++14 } }
// { dg-options "-O2 -Wall" }

struct __new_allocator {
  constexpr __new_allocator() {}
  __new_allocator(__new_allocator &) {}
};
template <typename> using __allocator_base = __new_allocator;
template <typename> struct allocator_traits;
template <typename> struct allocator : __allocator_base<int> {};
template <typename _Tp> struct allocator_traits<allocator<_Tp>> {
  using pointer = _Tp *;
  template <typename _Up> using rebind_alloc = allocator<_Up>;
  static void deallocate(allocator<_Tp>, pointer, long);
};
struct __alloc_traits : allocator_traits<allocator<int>> {};
struct _Vector_impl_data {
  __alloc_traits::pointer _M_start;
  __alloc_traits::pointer _M_end_of_storage;
  constexpr _Vector_impl_data() : _M_start(), _M_end_of_storage() {}
};
struct _Vector_impl : __alloc_traits::rebind_alloc<int>, _Vector_impl_data {};
struct _Vector_base {
  ~_Vector_base() {
    _M_deallocate(_M_impl._M_start,
                  _M_impl._M_end_of_storage - _M_impl._M_start);
  }
  _Vector_impl _M_impl;
  void _M_deallocate(__alloc_traits::pointer __p, long __n) {
    if (__p)
      __alloc_traits::deallocate(_M_impl, __p, __n);
  }
};
struct vector : protected _Vector_base {};
struct S {
  vector a{};
};
struct B2 {
  B2(S);
};
struct E : B2 {
  E(S opts = {}) : B2{opts} {}
};
void fun() { E{}; }
