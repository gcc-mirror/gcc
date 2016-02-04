// { dg-options "-O2 -flto -w" }
template <typename> class allocator;
template <typename _CharT, typename = _CharT, typename = allocator<_CharT>>
class Trans_NS___cxx11_basic_string;
struct __false_type {};
template <typename> using __void_t = void;
template <typename, typename, template <typename...> class, typename...>
struct __detector {
  using type = int;
};
template <typename _Default, template <typename...> class _Op,
          typename... _Args>
struct __detector<_Default, __void_t<_Op<_Args...>>, _Op, _Args...> {
  using type = _Op<_Args...>;
};
template <typename _Default, template <typename...> class _Op,
          typename... _Args>
using __detected_or = __detector<_Default, void, _Op, _Args...>;
template <typename _Default, template <typename...> class _Op,
          typename... _Args>
using __detected_or_t = typename __detected_or<_Default, _Op, _Args...>::type;
template <template <typename...> class _Default,
          template <typename...> class _Op, typename... _Args>
using __detected_or_t_ = __detected_or_t<_Default<_Args...>, _Op, _Args...>;
struct random_access_iterator_tag {};
class __undefined;
template <typename, typename> using __replace_first_arg_t = __undefined;
template <typename> class allocator {
public:
  template <typename> struct rebind { typedef allocator other; };
};
struct __allocator_traits_base {
  template <typename _Alloc, typename _Up>
  using __rebind = typename _Alloc::template rebind<_Up>::other;
  template <typename _Tp> using __pointer = typename _Tp::pointer;
  template <typename _Tp> using __size_type = typename _Tp::size_type;
};
template <typename _Alloc, typename _Up>
using __alloc_rebind =
    __detected_or_t_<__replace_first_arg_t, __allocator_traits_base::__rebind,
                     _Alloc, _Up>;
struct allocator_traits : __allocator_traits_base {
  using pointer = __detected_or_t<char, __pointer, allocator<char>>;
  using size_type = __detected_or_t<int, __size_type, allocator<char>>;
  template <typename _Tp>
  using rebind_alloc = __alloc_rebind<allocator<char>, _Tp>;
};
struct __alloc_traits : allocator_traits {
  struct rebind {
    typedef rebind_alloc<int> other;
  };
};
template <typename, typename, typename> class Trans_NS___cxx11_basic_string {
public:
  struct _Alloc_hider : __alloc_traits::rebind::other {
    _Alloc_hider(__alloc_traits::pointer, allocator);
  } _M_dataplus;
  __alloc_traits::pointer _M_local_data();
  template <typename _InIterator>
  void _M_construct_aux(_InIterator __beg, _InIterator __end, __false_type) {
    _M_construct(__beg, __end, random_access_iterator_tag());
  }
  template <typename _InIterator>
  void _M_construct(_InIterator __beg, _InIterator __end) {
    _M_construct_aux(__beg, __end, __false_type());
  }
  template <typename _FwdIterator>
  void _M_construct(_FwdIterator, _FwdIterator, random_access_iterator_tag);
  char _S_copy___s2;
  void _S_copy(char *, __alloc_traits::size_type __n) {
    __builtin_memcpy(0, &_S_copy___s2, __n);
  }
  template <class _Iterator>
  void _S_copy_chars(char *__p, _Iterator __k1, _Iterator __k2) {
    _S_copy(__p, __k2 - __k1);
  }
  Trans_NS___cxx11_basic_string(char *__s,
                                allocator<char> __a = allocator<char>())
      : _M_dataplus(_M_local_data(), __a) {
    _M_construct(__s, __s);
  }
};
template <typename _CharT, typename _Traits, typename _Alloc>
template <typename _InIterator>
void Trans_NS___cxx11_basic_string<_CharT, _Traits, _Alloc>::_M_construct(
    _InIterator __beg, _InIterator __end, random_access_iterator_tag) {
  _S_copy_chars(0, __beg, __end);
}
class Decimal {
  Trans_NS___cxx11_basic_string<char> toString() const;
};
Trans_NS___cxx11_basic_string<char> Decimal::toString() const {
  return "Infinity";
}
