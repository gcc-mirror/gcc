// { dg-do compile }
// { dg-additional-options "-fnon-call-exceptions -fno-inline-functions-called-once -fno-tree-sra --param early-inlining-insns=1" }
template <typename, typename> struct __replace_first_arg;
template <template <typename> class _Template, typename _Up, typename _Tp,
          typename... _Types>
struct __replace_first_arg<_Template<_Tp, _Types...>, _Up> {
  using type = _Template<_Up>;
};
template <class> class min_pointer;
struct pointer_traits {
  template <typename _Up>
  using rebind = typename __replace_first_arg<min_pointer<int>, _Up>::type;
};
template <typename, typename _Tp>
using __ptr_rebind = pointer_traits::rebind<_Tp>;
template <typename _Alloc> struct allocator_traits {
  template <typename _Tp>
  static auto construct(_Alloc, _Tp) noexcept -> decltype(0);
};
template <typename _ForwardIterator, typename _Allocator>
void _Destroy(_ForwardIterator __last, _Allocator) {
  _ForwardIterator __first;
  for (; __first != __last;)
    ;
}
template <typename _ForwardIterator, typename _Allocator>
void __uninitialized_default_a(_ForwardIterator __last, _Allocator __alloc) {
  _ForwardIterator __first;
  try {
    for (; __first != __last;)
      allocator_traits<_Allocator>::construct(__alloc, 0);
  } catch (...) {
    _Destroy(__first, __alloc);
  }
}
template <typename _Ptr> struct _Deque_iterator {
  typedef __ptr_rebind<_Ptr, __ptr_rebind<_Ptr, int>> _Map_pointer;
};
template <typename _Alloc> class _Deque_base {
protected:
  typedef _Alloc _Tp_alloc_type;
  typedef _Deque_iterator<typename _Tp_alloc_type ::pointer> iterator;
  _Deque_base(_Alloc, long);
  typedef typename iterator::_Map_pointer _Map_pointer;
  _Tp_alloc_type _M_get_Tp_allocator();
};
template <typename _Alloc> class deque : _Deque_base<_Alloc> {
  typedef _Deque_base<_Alloc> _Base;
  typedef typename _Base::_Map_pointer _Map_pointer;
  typedef typename _Base::iterator iterator;
  using _Base::_M_get_Tp_allocator;

public:
  deque(int, _Alloc __a) : _Base(__a, 0) {
    _Map_pointer __cur;
    try {
      __uninitialized_default_a(__cur, _M_get_Tp_allocator());
    } catch (...) {
    }
    _M_destroy_data(begin(), end(), 0);
  }
  iterator begin();
  iterator end();
  template <typename _Alloc1>
  void _M_destroy_data(iterator, iterator, _Alloc1) {
    for (_Map_pointer __node;;)
      _Destroy(__node, _M_get_Tp_allocator());
  }
};
template <class T> class min_pointer {
  T ptr_;
  friend bool operator==(min_pointer x, min_pointer y) {
    return x.ptr_ == y.ptr_;
  }
  friend bool operator!=(min_pointer x, min_pointer y) { return x == y; }
};
template <class> class min_allocator {
public:
  typedef int pointer;
};
int foo() {
  int n;
  min_allocator<int> alloc;
  deque<min_allocator<int>>(n, alloc);
  return 1;
}

