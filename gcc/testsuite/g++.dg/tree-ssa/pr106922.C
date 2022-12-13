// { dg-require-effective-target c++20 }
// { dg-options "-O2 -fdump-tree-dce7" }

template <typename> struct __new_allocator {
  void deallocate(int *, int) { operator delete(0); }
};
template <typename _Tp> using __allocator_base = __new_allocator<_Tp>;
template <typename> struct allocator : __allocator_base<int> {
  [[__gnu__::__always_inline__]] void deallocate(int *__p, int __n) {
    __allocator_base<int>::deallocate(__p, __n);
  }
};
template <typename> struct allocator_traits;
template <typename _Tp> struct allocator_traits<allocator<_Tp>> {
  using allocator_type = allocator<_Tp>;
  using pointer = _Tp *;
  using size_type = int;
  template <typename _Up> using rebind_alloc = allocator<_Up>;
  static void deallocate(allocator_type &__a, pointer __p, size_type __n) {
    __a.deallocate(__p, __n);
  }
};
template <typename _Alloc> struct __alloc_traits : allocator_traits<_Alloc> {
  typedef allocator_traits<_Alloc> _Base_type;
  template <typename _Tp> struct rebind {
    typedef _Base_type::template rebind_alloc<_Tp> other;
  };
};
long _M_deallocate___n;
struct _Vector_base {
  typedef __alloc_traits<allocator<int>>::rebind<int>::other _Tp_alloc_type;
  typedef __alloc_traits<_Tp_alloc_type>::pointer pointer;
  struct _Vector_impl_data {
    pointer _M_start;
  };
  struct _Vector_impl : _Tp_alloc_type, _Vector_impl_data {};
  ~_Vector_base() { _M_deallocate(_M_impl._M_start); }
  _Vector_impl _M_impl;
  void _M_deallocate(pointer __p) {
    if (__p)
      __alloc_traits<_Tp_alloc_type>::deallocate(_M_impl, __p,
                                                 _M_deallocate___n);
  }
};
struct vector : _Vector_base {};
struct aligned_storage {
  int dummy_;
  int *ptr_ref0;
  vector &ref() {
    vector *__trans_tmp_2;
    void *__trans_tmp_1 = &dummy_;
    union {
      void *ap_pvoid;
      vector *as_ptype;
    } caster{__trans_tmp_1};
    __trans_tmp_2 = caster.as_ptype;
    return *__trans_tmp_2;
  }
};
struct optional_base {
  optional_base operator=(optional_base &) {
    bool __trans_tmp_3 = m_initialized;
    if (__trans_tmp_3)
      m_initialized = false;
    return *this;
  }
  ~optional_base() {
    if (m_initialized)
      m_storage.ref().~vector();
  }
  bool m_initialized;
  aligned_storage m_storage;
};
struct optional : optional_base {
  optional() : optional_base() {}
};
template <class> using Optional = optional;
struct Trans_NS___cxx11_basic_stringstream {};
void operator<<(Trans_NS___cxx11_basic_stringstream, int);
int testfunctionfoo_myStructs[10];
void testfunctionfoo() {
  Optional<char> external, internal;
  for (auto myStruct : testfunctionfoo_myStructs) {
    Trans_NS___cxx11_basic_stringstream address_stream;
    address_stream << myStruct;
    external = internal;
  }
}

// { dg-final { scan-tree-dump-not "m_initialized" "dce7" } }
