// { dg-do compile }
// { dg-additional-options "-fcompare-debug" }

template <int> struct __conditional;
template <bool _Cond, typename, typename>
using __conditional_t = typename __conditional<_Cond>::type;
template <typename> struct __is_void_helper {};
template <typename _Tp> _Tp *__addressof(_Tp &__r) {
  return __builtin_addressof(__r);
}
template <typename _Tp> _Tp *addressof(_Tp &__r) { return __addressof(__r); }
template <typename _Tp>
using __make_not_void = __conditional_t<_Tp ::value, int, _Tp>;
template <typename> struct pointer_traits;
template <typename _Tp> struct pointer_traits<_Tp *> {
  typedef _Tp *pointer;
  typedef _Tp element_type;
  static pointer pointer_to(element_type &__r) { return addressof(__r); }
};
namespace {
template <typename> struct new_allocator;
}
template <typename> struct allocator_traits;
template <typename> struct allocator;
template <typename _Tp> struct allocator_traits<allocator<_Tp>> {
  using pointer = _Tp *;
  using const_pointer = _Tp *;
};
namespace __gnu_cxx {
template <typename = char>
struct __alloc_traits : allocator_traits<allocator<char>> {};
} // namespace __gnu_cxx
template <class> struct char_traits;
template <typename _CharT, typename = _CharT>
class Trans_NS___cxx11_basic_string;
template <> struct char_traits<char> {
  typedef char char_type;
  static void assign(char_type, char_type);
};
template <typename, typename> struct Trans_NS___cxx11_basic_string {
  typedef __gnu_cxx::__alloc_traits<> _Alloc_traits;
  typedef char_traits<char> traits_type;
  typedef _Alloc_traits::pointer pointer;
  typedef _Alloc_traits::const_pointer const_pointer;
  struct {
    pointer _M_p;
  } _M_dataplus;
  char _M_local_buf[];
  void _M_data(pointer __p) { _M_dataplus._M_p = __p; }
  bool _M_is_local() {
    const_pointer __trans_tmp_5 =
        pointer_traits<const_pointer>::pointer_to(*_M_local_buf);
    return _M_dataplus._M_p == __trans_tmp_5;
  }
  void operator=(Trans_NS___cxx11_basic_string __str) {
    bool __trans_tmp_2;
    if (__str._M_is_local()) {
      Trans_NS___cxx11_basic_string *__trans_tmp_1;
      if (__builtin_expect(__trans_tmp_1 != this, true))
        size();
    } else if (__trans_tmp_2)
      __str._M_data(__str._M_local_buf);
    __str.clear();
  }
  void size();
  void clear() { traits_type::assign(_M_dataplus._M_p[0], char()); }
};
template <class, typename, int> struct Pool {
  template <class T> struct PoolIterator {
    bool operator!=(PoolIterator);
    T *operator*();
    void operator++();
  };
  template <class T> struct IterateWrapper {
    PoolIterator<T> begin();
    PoolIterator<T> end();
  };
};
struct BaseConsist {
  Trans_NS___cxx11_basic_string<char> name;
};
struct Vehicle : BaseConsist {};
Pool<int, int, true>::IterateWrapper<Vehicle> __trans_tmp_4;
Trans_NS___cxx11_basic_string<char> __trans_tmp_6;
void FixOldVehicles() {
  for (Vehicle *v : __trans_tmp_4)
    v->name = __trans_tmp_6;
}
