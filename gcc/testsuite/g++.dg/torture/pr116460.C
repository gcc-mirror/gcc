// { dg-do compile }
// { dg-additional-options "-g" }

namespace std {
typedef __SIZE_TYPE__ size_t;
typedef __PTRDIFF_TYPE__ ptrdiff_t;
void __throw_length_error(const char *) __attribute__((__noreturn__, __cold__));
}
extern "C++" {
namespace std __attribute__((__visibility__("default"))) {
  template <typename _Tp> struct __is_integer {
    enum { __value = 1 };
  };
  template <typename _Tp> struct __is_nonvolatile_trivially_copyable {
    enum { __value = __is_trivially_copyable(_Tp) };
  };
  template <typename _OutputIter, typename _InputIter> struct __memcpyable {};
  template <typename _Tp>
  struct __memcpyable<_Tp *, _Tp *> : __is_nonvolatile_trivially_copyable<_Tp> {
  };
  template <typename _Tp>
  struct __memcpyable<_Tp *, const _Tp *>
      : __is_nonvolatile_trivially_copyable<_Tp> {};
  template <typename _Tp> struct __is_move_iterator {
    enum { __value = 0 };
  };
  template <typename _Iterator> inline _Iterator __miter_base(_Iterator __it) {
    return __it;
  }
} // namespace )
}
namespace __gnu_cxx __attribute__((__visibility__("default"))) {
  template <typename _Tp>
  struct __is_integer_nonstrict : public std::__is_integer<_Tp> {
    using std::__is_integer<_Tp>::__value;
    enum { __width = __value ? sizeof(_Tp) * 8 : 0 };
  };
  template <typename _Value> struct __numeric_traits_integer {
    static const bool __is_signed = (_Value)(-1) < 0;
    static const int __digits =
        __is_integer_nonstrict<_Value>::__width - __is_signed;
    static const _Value __max =
        __is_signed ? (((((_Value)1 << (__digits - 1)) - 1) << 1) + 1)
                    : ~(_Value)0;
  };
  template <typename _Value>
  struct __numeric_traits : public __numeric_traits_integer<_Value> {};
} // namespace )
namespace std __attribute__((__visibility__("default"))) {
  template <typename _Tp, _Tp __v> struct integral_constant {
    static constexpr _Tp value = __v;
    using type = integral_constant<_Tp, __v>;
  };
  template <bool __v> using __bool_constant = integral_constant<bool, __v>;
  using true_type = __bool_constant<true>;
  using false_type = __bool_constant<false>;
  template <bool, typename _Tp = void> struct enable_if {};
  template <typename _Tp> struct enable_if<true, _Tp> { using type = _Tp; };
  template <bool _Cond, typename _Tp = void>
  using __enable_if_t = typename enable_if<_Cond, _Tp>::type;
  template <bool> struct __conditional {
    template <typename _Tp, typename> using type = _Tp;
  };
  template <bool _Cond, typename _If, typename _Else>
  using __conditional_t =
      typename __conditional<_Cond>::template type<_If, _Else>;
  namespace __detail {
  template <typename... _Bn> auto __and_fn(...) -> false_type;
  }
  template <typename... _Bn>
  struct __and_ : decltype(__detail::__and_fn<_Bn...>(0)) {};
  template <typename _Pp> struct __not_ : __bool_constant<!bool(_Pp::value)> {};
  template <typename...> using __void_t = void;
  template <typename _Tp>
  struct is_trivial : public __bool_constant<__is_trivial(_Tp)> {};
  template <typename _Tp, typename _Up = _Tp &&> _Up __declval(int);
  template <typename _Tp> auto declval() noexcept->decltype(__declval<_Tp>(0));
  template <typename _Tp, typename... _Args>
  using __is_constructible_impl =
      __bool_constant<__is_constructible(_Tp, _Args...)>;
  template <typename _Tp, typename = void>
  struct __add_lvalue_reference_helper {
    using type = _Tp &;
  };
  template <typename _Tp>
  using __add_lval_ref_t = typename __add_lvalue_reference_helper<_Tp>::type;
  template <typename _Tp>
  struct is_copy_constructible
      : public __is_constructible_impl<_Tp, __add_lval_ref_t<const _Tp>> {};
  template <typename _Tp, typename = void>
  struct __add_rvalue_reference_helper {
    using type = _Tp;
  };
  template <typename _Tp>
  using __add_rval_ref_t = typename __add_rvalue_reference_helper<_Tp>::type;
  template <typename _Tp>
  struct is_move_constructible
      : public __is_constructible_impl<_Tp, __add_rval_ref_t<_Tp>> {};
  template <typename _Tp, typename... _Args>
  using __is_nothrow_constructible_impl =
      __bool_constant<__is_nothrow_constructible(_Tp, _Args...)>;
  template <typename _Tp>
  struct is_nothrow_default_constructible
      : public __is_nothrow_constructible_impl<_Tp> {};
  template <typename _Tp>
  struct is_nothrow_copy_constructible
      : public __is_nothrow_constructible_impl<_Tp,
                                               __add_lval_ref_t<const _Tp>> {};
  template <typename _Tp>
  struct is_nothrow_move_constructible
      : public __is_nothrow_constructible_impl<_Tp, __add_rval_ref_t<_Tp>> {};
  template <typename _Tp> struct remove_reference {
    using type = __remove_reference(_Tp);
  };
  template <typename _Tp>
  [[__nodiscard__]] constexpr _Tp &&forward(
      typename std::remove_reference<_Tp>::type & __t) noexcept {
    return static_cast<_Tp &&>(__t);
  }
  template <typename _Tp>
  [[__nodiscard__]] constexpr typename std::remove_reference<_Tp>::type &&move(
      _Tp && __t) noexcept {
    return static_cast<typename std::remove_reference<_Tp>::type &&>(__t);
  }
  template <typename _Tp>
  struct __move_if_noexcept_cond
      : public __and_<__not_<is_nothrow_move_constructible<_Tp>>,
                      is_copy_constructible<_Tp>>::type {};
  struct input_iterator_tag {};
  struct forward_iterator_tag : public input_iterator_tag {};
  struct bidirectional_iterator_tag : public forward_iterator_tag {};
  struct random_access_iterator_tag : public bidirectional_iterator_tag {};
  template <typename _Iterator, typename = __void_t<>>
  struct __iterator_traits {
    typedef typename _Iterator::value_type value_type;
  };
  template <typename _Iterator>
  struct iterator_traits : public __iterator_traits<_Iterator> {};
  template <typename _Tp> struct iterator_traits<_Tp *> {
    typedef random_access_iterator_tag iterator_category;
    typedef _Tp value_type;
    typedef ptrdiff_t difference_type;
    typedef _Tp &reference;
  };
  template <typename _Iter>
  typename iterator_traits<_Iter>::iterator_category __iterator_category(
      const _Iter &) {
    return typename iterator_traits<_Iter>::iterator_category();
  }
  template <typename _RandomAccessIterator>
  typename iterator_traits<_RandomAccessIterator>::difference_type __distance(
      _RandomAccessIterator __first, _RandomAccessIterator __last,
      random_access_iterator_tag) {
    return __last - __first;
  }
  template <typename _InputIterator>
  typename iterator_traits<_InputIterator>::difference_type distance(
      _InputIterator __first, _InputIterator __last) {
    return std::__distance(__first, __last, std::__iterator_category(__first));
  }
} // namespace )
namespace __gnu_cxx __attribute__((__visibility__("default"))) {
  template <typename _Iterator, typename _Container> class __normal_iterator {
    _Iterator _M_current;
    typedef std::iterator_traits<_Iterator> __traits_type;
  public:
    typedef typename __traits_type::value_type value_type;
    typedef typename __traits_type::difference_type difference_type;
    typedef typename __traits_type::reference reference;
    explicit __normal_iterator(const _Iterator &__i) noexcept
        : _M_current(__i) {}
    reference operator*() const noexcept { return *_M_current; }
    __normal_iterator &operator++() noexcept {
      ++_M_current;
      return *this;
    }
    __normal_iterator &operator--() noexcept {
      --_M_current;
      return *this;
    }
    __normal_iterator operator+(difference_type __n) const noexcept {
      return __normal_iterator(_M_current + __n);
    }
    __normal_iterator operator-(difference_type __n) const noexcept {
      return __normal_iterator(_M_current - __n);
    }
    const _Iterator &base() const noexcept { return _M_current; }
  };
  template <typename _IteratorL, typename _IteratorR, typename _Container>
  [[__nodiscard__]] inline bool operator!=(
      const __normal_iterator<_IteratorL, _Container> &__lhs,
      const __normal_iterator<_IteratorR, _Container> &__rhs) noexcept {
    return __lhs.base() != __rhs.base();
  }
  template <typename _Iterator, typename _Container>
  typename __normal_iterator<_Iterator, _Container>::difference_type operator-(
      const __normal_iterator<_Iterator, _Container> &__lhs,
      const __normal_iterator<_Iterator, _Container> &__rhs) noexcept {
    return __lhs.base() - __rhs.base();
  }
} // namespace )
namespace std __attribute__((__visibility__("default"))) {
} // namespace )
namespace __gnu_cxx {
namespace __ops {
template <typename _Compare> struct _Iter_comp_iter {
  _Compare _M_comp;
  explicit constexpr _Iter_comp_iter(_Compare __comp)
      : _M_comp(std::move(__comp)) {}
  template <typename _Iterator1, typename _Iterator2>
  constexpr bool operator()(_Iterator1 __it1, _Iterator2 __it2) {
    return bool(_M_comp(*__it1, *__it2));
  }
};
template <typename _Compare>
constexpr inline _Iter_comp_iter<_Compare> __iter_comp_iter(_Compare __comp) {
  return _Iter_comp_iter<_Compare>(std::move(__comp));
}
template <typename _Compare> struct _Val_comp_iter {
  _Compare _M_comp;
  explicit _Val_comp_iter(const _Iter_comp_iter<_Compare> &__comp)
      : _M_comp(std::move(__comp._M_comp)) {}
  template <typename _Value, typename _Iterator>
  bool operator()(_Value &__val, _Iterator __it) {
    return bool(_M_comp(__val, *__it));
  }
};
template <typename _Compare>
inline _Val_comp_iter<_Compare>
__val_comp_iter(_Iter_comp_iter<_Compare> __comp) {
  return _Val_comp_iter<_Compare>(std::move(__comp));
}
} // namespace __ops
} // namespace __gnu_cxx
namespace std __attribute__((__visibility__("default"))) {
  template <typename _Tp>
  [[__nodiscard__]] constexpr inline const _Tp &min(const _Tp &__a,
                                                    const _Tp &__b) {
    return __a;
  }
  template <typename _Tp>
  [[__nodiscard__]] constexpr inline const _Tp &max(const _Tp &__a,
                                                    const _Tp &__b) {
    return __b;
  }
  template <typename _Iterator>
  inline _Iterator __niter_base(_Iterator __it) noexcept(
      std::is_nothrow_copy_constructible<_Iterator>::value) {
    return __it;
  }
  template <typename _Iterator>
  inline _Iterator __niter_wrap(const _Iterator &, _Iterator __res) {
    return __res;
  }
  template <bool _IsMove, bool _IsSimple, typename _Category>
  struct __copy_move {
    template <typename _Tp, typename _Up>
    static _Up *__copy_m(_Tp *__first, _Tp *__last, _Up *__result) {
      const ptrdiff_t _Num = __last - __first;
      return __result + _Num;
    }
  };
  template <bool _IsMove, typename _II, typename _OI>
  inline _OI __copy_move_a2(_II __first, _II __last, _OI __result) {
    typedef typename iterator_traits<_II>::iterator_category _Category;
    return std::__copy_move<_IsMove, __memcpyable<_OI, _II>::__value,
                            _Category>::__copy_m(__first, __last, __result);
  }
  template <bool _IsMove, typename _II, typename _OI>
  inline _OI __copy_move_a1(_II __first, _II __last, _OI __result) {
    return std::__copy_move_a2<_IsMove>(__first, __last, __result);
  }
  template <bool _IsMove, typename _II, typename _OI>
  inline _OI __copy_move_a(_II __first, _II __last, _OI __result) {
    return std::__niter_wrap(
        __result, std::__copy_move_a1<_IsMove>(std::__niter_base(__first),
                                               std::__niter_base(__last),
                                               std::__niter_base(__result)));
  }
  template <typename _II, typename _OI>
  inline _OI copy(_II __first, _II __last, _OI __result) {
    return std::__copy_move_a<__is_move_iterator<_II>::__value>(
        std::__miter_base(__first), std::__miter_base(__last), __result);
  }
  template <typename _Tp> class __new_allocator {
  public:
    typedef _Tp value_type;
    typedef std::size_t size_type;
    __attribute__((__always_inline__)) [[__nodiscard__]] _Tp *
    allocate(size_type __n, const void * = static_cast<const void *>(0)) {
      return static_cast<_Tp *>(::operator new(__n * sizeof(_Tp)));
    }
    __attribute__((__always_inline__)) size_type max_size() const noexcept {
      return _M_max_size();
    }
    __attribute__((__always_inline__)) constexpr size_type
    _M_max_size() const noexcept {
      return std::size_t(0x7fffffffffffffffL) / sizeof(_Tp);
    }
  };
  template <typename _Tp> using __allocator_base = __new_allocator<_Tp>;
  template <typename> struct allocator_traits;
  template <typename _Tp> class allocator : public __allocator_base<_Tp> {};
  template <typename _Tp> struct allocator_traits<allocator<_Tp>> {
    using allocator_type = allocator<_Tp>;
    using value_type = _Tp;
    using pointer = _Tp *;
    using size_type = std::size_t;
    template <typename _Up> using rebind_alloc = allocator<_Up>;
    [[__nodiscard__, __gnu__::__always_inline__]] static pointer
    allocate(allocator_type &__a, size_type __n) {
      return __a.allocate(__n);
    }
    [[__gnu__::__always_inline__]] static size_type
    max_size(const allocator_type &__a __attribute__((__unused__))) noexcept {
      return __a.max_size();
    }
  };
  template <typename _Alloc, typename _Tp, typename = void>
  struct __is_alloc_insertable_impl : false_type {};
  template <typename _Alloc>
  struct __is_move_insertable
      : __is_alloc_insertable_impl<_Alloc, typename _Alloc::value_type>::type {
  };
  template <typename _Tp>
  struct __is_move_insertable<allocator<_Tp>> : is_move_constructible<_Tp> {};
} // namespace )
namespace __gnu_cxx __attribute__((__visibility__("default"))) {
  template <typename _Alloc, typename = typename _Alloc::value_type>
  struct __alloc_traits : std::allocator_traits<_Alloc> {
    typedef std::allocator_traits<_Alloc> _Base_type;
    typedef typename _Base_type::value_type value_type;
    typedef value_type &reference;
    template <typename _Tp> struct rebind {
      typedef typename _Base_type::template rebind_alloc<_Tp> other;
    };
  };
} // namespace )
namespace std __attribute__((__visibility__("default"))) {
  template <typename _ValueType, typename _Tp>
  constexpr bool __check_constructible() {
    return true;
  }
  template <bool _TrivialValueTypes> struct __uninitialized_copy {
    template <typename _InputIterator, typename _ForwardIterator>
    static _ForwardIterator __uninit_copy(_InputIterator __first,
                                          _InputIterator __last,
                                          _ForwardIterator __result) {
      return std::copy(__first, __last, __result);
    }
  };
  template <typename _InputIterator, typename _ForwardIterator>
  inline _ForwardIterator uninitialized_copy(_InputIterator __first,
                                             _InputIterator __last,
                                             _ForwardIterator __result) {
    typedef typename iterator_traits<_InputIterator>::value_type _ValueType1;
    typedef typename iterator_traits<_ForwardIterator>::value_type _ValueType2;
    const bool __can_memmove = __is_trivial(_ValueType1);
    using _From = decltype(*__first);
    const bool __assignable = __is_trivial(_ValueType2) &&
                              std::__check_constructible<_ValueType2, _From>();
    return std::__uninitialized_copy < __can_memmove &&
           __assignable > ::__uninit_copy(__first, __last, __result);
  }
  template <typename _InputIterator, typename _ForwardIterator, typename _Tp>
  inline _ForwardIterator __uninitialized_copy_a(
      _InputIterator __first, _InputIterator __last, _ForwardIterator __result,
      allocator<_Tp> &) {
    return std::uninitialized_copy(__first, __last, __result);
  }
  template <typename _InputIterator, typename _ForwardIterator,
            typename _Allocator>
  inline _ForwardIterator __uninitialized_move_if_noexcept_a(
      _InputIterator __first, _InputIterator __last, _ForwardIterator __result,
      _Allocator & __alloc) {
    return std::__uninitialized_copy_a(
        __first,
        __last, __result, __alloc);
  }
  template <typename _Tp, typename = void>
  struct __is_bitwise_relocatable : is_trivial<_Tp> {};
  template <typename _Tp, typename _Up>
  inline __enable_if_t<std::__is_bitwise_relocatable<_Tp>::value, _Tp *>
  __relocate_a_1(_Tp * __first, _Tp * __last, _Tp * __result,
                 [[__maybe_unused__]] allocator<_Up> & __alloc) noexcept {}
  template <typename _InputIterator, typename _ForwardIterator,
            typename _Allocator>
  inline _ForwardIterator __relocate_a(
      _InputIterator __first, _InputIterator __last, _ForwardIterator __result,
      _Allocator &
          __alloc) noexcept(noexcept(__relocate_a_1(std::__niter_base(__first),
                                                    std::__niter_base(__last),
                                                    std::__niter_base(__result),
                                                    __alloc))) {}
  template <class _E> class initializer_list {
    typedef size_t size_type;
    typedef const _E *iterator;
    typedef const _E *const_iterator;
    iterator _M_array;
    size_type _M_len;
  public:
    constexpr size_type size() const noexcept { return _M_len; }
    constexpr const_iterator begin() const noexcept { return _M_array; }
    constexpr const_iterator end() const noexcept { return begin() + size(); }
  };
  template <typename _Tp, typename _Alloc> struct _Vector_base {
    typedef
        typename __gnu_cxx::__alloc_traits<_Alloc>::template rebind<_Tp>::other
            _Tp_alloc_type;
    typedef typename __gnu_cxx::__alloc_traits<_Tp_alloc_type>::pointer pointer;
    struct _Vector_impl_data {
      pointer _M_start;
      pointer _M_finish;
      pointer _M_end_of_storage;
      _Vector_impl_data() noexcept
          : _M_start(), _M_finish(), _M_end_of_storage() {}
    };
    struct _Vector_impl : public _Tp_alloc_type, public _Vector_impl_data {
      _Vector_impl() noexcept(
          is_nothrow_default_constructible<_Tp_alloc_type>::value)
          : _Tp_alloc_type() {}
      _Vector_impl(_Tp_alloc_type const &__a) noexcept : _Tp_alloc_type(__a) {}
      _Vector_impl(_Tp_alloc_type &&__a, _Vector_impl &&__rv) noexcept
          : _Tp_alloc_type(std::move(__a)), _Vector_impl_data(std::move(__rv)) {
      }
    };
    typedef _Alloc allocator_type;
    _Tp_alloc_type &_M_get_Tp_allocator() noexcept { return this->_M_impl; }
    const _Tp_alloc_type &_M_get_Tp_allocator() const noexcept {
      return this->_M_impl;
    }
    _Vector_base() = default;
    _Vector_base(const allocator_type &__a) noexcept : _M_impl(__a) {}
    _Vector_impl _M_impl;
    pointer _M_allocate(size_t __n) {
      typedef __gnu_cxx::__alloc_traits<_Tp_alloc_type> _Tr;
      return __n != 0 ? _Tr::allocate(_M_impl, __n) : pointer();
    }
  };
  template <typename _Tp, typename _Alloc = std::allocator<_Tp>>
  class vector : protected _Vector_base<_Tp, _Alloc> {
    typedef _Vector_base<_Tp, _Alloc> _Base;
    typedef typename _Base::_Tp_alloc_type _Tp_alloc_type;
    typedef __gnu_cxx::__alloc_traits<_Tp_alloc_type> _Alloc_traits;
    typedef _Tp value_type;
    typedef typename _Base::pointer pointer;
    typedef typename _Alloc_traits::reference reference;
    typedef __gnu_cxx::__normal_iterator<pointer, vector> iterator;
    typedef size_t size_type;
    typedef _Alloc allocator_type;
    static constexpr bool _S_nothrow_relocate(true_type) {
      return noexcept(std::__relocate_a(
          std::declval<pointer>(), std::declval<pointer>(),
          std::declval<pointer>(), std::declval<_Tp_alloc_type &>()));
    }
    static constexpr bool _S_use_relocate() {
      return _S_nothrow_relocate(__is_move_insertable<_Tp_alloc_type>{});
    }
    using _Base::_M_get_Tp_allocator;
  public:
    vector() = default;
    vector(initializer_list<value_type> __l,
           const allocator_type &__a = allocator_type())
        : _Base(__a) {
      _M_range_initialize(__l.begin(), __l.end(), random_access_iterator_tag());
    }
    [[__nodiscard__]] iterator begin() noexcept {
      return iterator(this->_M_impl._M_start);
    }
    [[__nodiscard__]] iterator end() noexcept {
      return iterator(this->_M_impl._M_finish);
    }
    [[__nodiscard__]] size_type size() const noexcept {
      return size_type(this->_M_impl._M_finish - this->_M_impl._M_start);
    }
    [[__nodiscard__]] size_type max_size() const noexcept {
      return _S_max_size(_M_get_Tp_allocator());
    }
    [[__nodiscard__]] reference back() noexcept { return *(end() - 1); }
    void push_back(value_type &&__x) { emplace_back(std::move(__x)); }
    template <typename... _Args> reference emplace_back(_Args &&...__args);
    template <typename _ForwardIterator>
    void _M_range_initialize(_ForwardIterator __first, _ForwardIterator __last,
                             std::forward_iterator_tag) {
      const size_type __n = std::distance(__first, __last);
      this->_M_allocate(_S_check_init_len(__n, _M_get_Tp_allocator()));
      this->_M_impl._M_finish = std::__uninitialized_copy_a(
          __first, __last, this->_M_impl._M_start, _M_get_Tp_allocator());
    }
    template <typename... _Args> void _M_realloc_append(_Args &&...__args);
    size_type _M_check_len(size_type __n, const char *__s) const {
      const size_type __len = size() + (std::max)(size(), __n);
      return (__len < size() || __len > max_size()) ? max_size() : __len;
    }
    static size_type _S_check_init_len(size_type __n,
                                       const allocator_type &__a) {
      if (__n > _S_max_size(_Tp_alloc_type(__a)))
        __throw_length_error(
            ("cannot create std::vector larger than max_size()"));
      return __n;
    }
    static size_type _S_max_size(const _Tp_alloc_type &__a) noexcept {
      const size_t __diffmax =
          __gnu_cxx::__numeric_traits<ptrdiff_t>::__max / sizeof(_Tp);
      return __diffmax;
    }
  };

  template <typename _Tp>
  struct greater  {
    constexpr bool operator()(const _Tp &__x, const _Tp &__y) const {
      return __x > __y;
    }
  };
  template <typename _Tp, typename _Alloc>
  template <typename... _Args>
  typename vector<_Tp, _Alloc>::reference vector<_Tp, _Alloc>::emplace_back(
      _Args && ...__args) {
    if (this->_M_impl._M_finish != this->_M_impl._M_end_of_storage) {
      ++this->_M_impl._M_finish;
    } else
      _M_realloc_append(std::forward<_Args>(__args)...);
    return back();
  }
  template <typename _Tp, typename _Alloc>
  template <typename... _Args>
  void vector<_Tp, _Alloc>::_M_realloc_append(_Args && ...__args) {
    const size_type __len = _M_check_len(1u, "vector::_M_realloc_append");
    pointer __old_start = this->_M_impl._M_start;
    pointer __old_finish = this->_M_impl._M_finish;
    pointer __new_start(this->_M_allocate(__len));
    pointer __new_finish(__new_start);
    {
      if constexpr (_S_use_relocate()) {
        __new_finish = std::__uninitialized_move_if_noexcept_a(
            __old_start, __old_finish, __new_start, _M_get_Tp_allocator());
      }
    }
    this->_M_impl._M_start = __new_start;
    this->_M_impl._M_finish = __new_finish;
    this->_M_impl._M_end_of_storage = __new_start + __len;
  }
  template <typename _RandomAccessIterator, typename _Compare>
  void __unguarded_linear_insert(_RandomAccessIterator __last,
                                 _Compare __comp) {
    typename iterator_traits<_RandomAccessIterator>::value_type __val =
        std::move(*__last);
    _RandomAccessIterator __next = __last;
    --__next;
    while (__comp(__val, __next)) {
      *__last = std::move(*__next);
      __last = __next;
    }
  }
  template <typename _RandomAccessIterator, typename _Compare>
  void __insertion_sort(_RandomAccessIterator __first,
                        _RandomAccessIterator __last, _Compare __comp) {
    for (_RandomAccessIterator __i = __first + 1; __i != __last; ++__i) {
      std::__unguarded_linear_insert(__i,
                                     __gnu_cxx::__ops::__val_comp_iter(__comp));
    }
  }
  template <typename _RandomAccessIterator, typename _Compare>
  inline void __unguarded_insertion_sort(_RandomAccessIterator __first,
                                         _RandomAccessIterator __last,
                                         _Compare __comp) {
    for (_RandomAccessIterator __i = __first; __i != __last; ++__i)
      std::__unguarded_linear_insert(__i,
                                     __gnu_cxx::__ops::__val_comp_iter(__comp));
  }
  enum { _S_threshold = 16 };
  template <typename _RandomAccessIterator, typename _Compare>
  void __final_insertion_sort(_RandomAccessIterator __first,
                              _RandomAccessIterator __last, _Compare __comp) {
    if (__last - __first > int(_S_threshold)) {
      std::__insertion_sort(__first, __first + int(_S_threshold), __comp);
      std::__unguarded_insertion_sort(__first + int(_S_threshold), __last,
                                      __comp);
    }
  }
  template <typename _RandomAccessIterator, typename _Compare>
  inline void __sort(_RandomAccessIterator __first,
                     _RandomAccessIterator __last, _Compare __comp) {
    if (__first != __last) {
      std::__final_insertion_sort(__first, __last, __comp);
    }
  }
  template <typename _RandomAccessIterator, typename _Compare>
  inline void sort(_RandomAccessIterator __first, _RandomAccessIterator __last,
                   _Compare __comp) {
    std::__sort(__first, __last, __gnu_cxx::__ops::__iter_comp_iter(__comp));
  }
}


void g();

void f(int nBands, double maxZErr) {
  for (int iBand = 0; iBand < nBands; iBand++)
   {
    g();
    std::vector<signed char> noDataCandVec;
    std::vector<signed char> distCandVec = {0, 1, 10, 100, 5, 6};
    for (signed char dist : distCandVec)
      noDataCandVec.push_back(1);
    std::sort(noDataCandVec.begin(), noDataCandVec.end(),
              std::greater<double>());
  }
}
