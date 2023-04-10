// { dg-lto-do link }
// { dg-require-effective-target shared }
// { dg-require-effective-target fpic }
// { dg-lto-options { "-flto -fPIC -shared -O1 -fimplicit-constexpr -g1" } }
// { dg-extra-ld-options "-shared" }

namespace std {
struct _Sp_counted_base {
  virtual void *_M_get_deleter(const int &);
};
bool _S_eq(int);
struct _Sp_make_shared_tag {
  static const int &_S_ti() {
    static constexpr char __tag{};
    return reinterpret_cast<const int &>(__tag);
  }
};
struct _Impl {
  _Impl(int);
};
int _Sp_counted_ptr_inplace___a;
struct _Sp_counted_ptr_inplace : _Sp_counted_base {
  _Sp_counted_ptr_inplace() : _M_impl(_Sp_counted_ptr_inplace___a) {}
  void *_M_get_deleter(const int &__ti) {
    auto __ptr(_M_ptr());
    &__ti == &_Sp_make_shared_tag::_S_ti() || _S_eq(__ti);
    return __ptr;
  }
  int *_M_ptr();
  _Impl _M_impl;
};
struct __shared_count {
  __shared_count(int, int) { _Sp_counted_ptr_inplace(); }
};
int _M_ptr;
struct __shared_ptr {
  template <typename _Alloc>
  __shared_ptr(_Alloc __tag) : _M_refcount(_M_ptr, __tag) {}
  __shared_count _M_refcount;
};
int shared_ptr___tag;
struct shared_ptr : __shared_ptr {
  shared_ptr() : __shared_ptr(shared_ptr___tag) {}
};
void ArgEq() { shared_ptr(); }
} // namespace std
