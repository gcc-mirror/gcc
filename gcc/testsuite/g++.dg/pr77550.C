// { dg-do run }
// { dg-options "-std=c++14 -O3" }

#define enum enum __attribute((mode(SI)))
namespace std {
typedef int size_t;
inline namespace __cxx11 {}
template <typename...> using _Require = void;
template <typename> using __void_t = void;
template <typename, typename, template <typename...> class, typename...>
struct A {
  using type = int;
};
template <typename _Default, template <typename...> class _Op,
          typename... _Args>
struct A<_Default, __void_t<_Op<_Args...>>, _Op, _Args...> {
  using type = _Op<_Args...>;
};
template <typename _Default, template <typename...> class _Op,
          typename... _Args>
using __detected_or = A<_Default, void, _Op, _Args...>;
template <typename _Default, template <typename...> class _Op,
          typename... _Args>
using __detected_or_t = typename __detected_or<_Default, _Op, _Args...>::type;
template <template <typename...> class _Default,
          template <typename...> class _Op, typename... _Args>
using __detected_or_t_ = __detected_or_t<_Default<_Args...>, _Op, _Args...>;
template <typename _InputIterator> void __distance(_InputIterator p1) { ++p1; }
template <typename _InputIterator>
void distance(_InputIterator p1, _InputIterator) {
  __distance(p1);
}
template <typename, typename> using __replace_first_arg_t = int;
struct B {
  template <typename _Up> using rebind = _Up *;
};
template <typename, typename> using __ptr_rebind = B;
template <typename _Tp> _Tp max(_Tp p1, _Tp) { return p1; }
}
void *operator new(__SIZE_TYPE__, void *p2) { return p2; }
template <typename _Tp> struct C {
  typedef _Tp *pointer;
  pointer allocate(int p1) {
    return static_cast<_Tp *>(operator new(p1 * sizeof(_Tp)));
  }
  template <typename _Up> void construct(_Up *p1) { new (p1) _Up; }
};
namespace std {
template <typename _Tp> using __allocator_base = C<_Tp>;
template <typename _Tp> struct allocator : __allocator_base<_Tp> {
  typedef __SIZE_TYPE__ size_type;
  template <typename _Tp1> struct rebind { typedef allocator<_Tp1> other; };
};
struct D {
  template <typename _Alloc, typename _Up>
  using __rebind = typename _Alloc::template rebind<_Up>::other;
  template <typename _Tp> using __pointer = typename _Tp::pointer;
  template <typename _Tp> using __c_pointer = typename _Tp::const_pointer;
  template <typename _Tp> using __size_type = typename _Tp::size_type;
};
template <typename _Alloc, typename _Up>
using __alloc_rebind =
    __detected_or_t_<__replace_first_arg_t, D::__rebind, _Alloc, _Up>;
template <typename _Alloc> struct K : D {
  typedef _Alloc value_type;
  using pointer = __detected_or_t<value_type, __pointer, _Alloc>;
  using const_pointer =
      __detected_or_t<__ptr_rebind<pointer, value_type>, __c_pointer>;
  using size_type = __detected_or_t<int, __size_type, _Alloc>;
  template <typename _Tp> using rebind_alloc = __alloc_rebind<_Alloc, _Tp>;
  template <typename _Tp> static _Require<> _S_construct(_Tp p1) {
    _Alloc __a;
    __a.construct(p1);
  }
  static pointer allocate(_Alloc p1, size_type p2) { return p1.allocate(p2); }
  template <typename _Tp, typename _Args>
  static auto construct(_Alloc, _Tp p2, _Args) {
    _S_construct(p2);
  }
};
}
template <typename _Alloc> struct O : std::K<_Alloc> {
  template <typename _Tp> struct rebind {
    typedef typename std::K<_Alloc>::template rebind_alloc<_Tp> other;
  };
};
namespace std {
template <typename _ForwardIterator, typename _Tp, typename _Allocator>
void __uninitialized_fill_a(_ForwardIterator p1, _ForwardIterator, _Tp,
                            _Allocator p4) try {
  O<_Allocator>::construct(p4, p1, 0);
} catch (...) {
}
size_t __deque_buf_size(size_t p1) { return 1 ? 512 / p1 : 0; }
template <typename _Tp, typename _Ref, typename> struct F {
  template <typename _Up> using __ptr_to = B::rebind<_Up>;
  template <typename _CvTp> using __iter = F<_Tp, _CvTp &, __ptr_to<_CvTp>>;
  typedef __ptr_to<_Tp> _Elt_pointer;
  typedef __ptr_to<_Elt_pointer> _Map_pointer;
  _Elt_pointer _M_cur;
  _Elt_pointer _M_first;
  _Elt_pointer _M_last;
  _Map_pointer _M_node;
  F() {}
  F(__iter<_Tp> &p1) : _M_cur(p1._M_cur), _M_node(p1._M_node) {}
  _Ref operator*() { return *_M_cur; }
  void operator++() {
    _M_set_node(_M_node + 1);
    _M_cur = _M_first;
  }
  void _M_set_node(_Map_pointer p1) {
    _M_node = p1;
    _M_first = *p1;
    _M_last = _M_first;
  }
};
template <typename _Tp, typename _Ref, typename _Ptr>
int operator==(F<_Tp, _Ref, _Ptr> p1, F<_Tp, _Ref, _Ptr> p2) {
  return p1._M_cur == p2._M_cur;
}
template <typename _Tp, typename _Ref, typename _Ptr>
int operator!=(F<_Tp, _Ref, _Ptr> p1, F<_Tp, _Ref, _Ptr> p2) {
  return !(p1 == p2);
}
template <typename _Tp, typename _Alloc> struct _Deque_base {
  typedef typename _Alloc::template rebind<_Tp>::other _Tp_alloc_type;
  typedef O<_Tp_alloc_type> _Alloc_traits;
  typedef typename _Alloc_traits::pointer _Ptr;
  typedef typename _Alloc_traits::template rebind<_Ptr>::other _Map_alloc_type;
  typedef F<_Tp, _Tp &, _Ptr> iterator;
  typedef F<_Tp, _Tp &, typename _Alloc_traits::const_pointer> const_iterator;
  _Deque_base(_Alloc p1, size_t) : _M_impl(p1) { _M_initialize_map(0); }
  ~_Deque_base() noexcept;
  typedef typename iterator::_Map_pointer _Map_pointer;
  struct L : _Tp_alloc_type {
    _Map_pointer _M_map;
    size_t _M_map_size;
    iterator _M_start;
    iterator _M_finish;
    L(_Tp_alloc_type) {}
  };
  _Tp_alloc_type _M_get_Tp_allocator() { return _M_impl; }
  _Ptr _M_allocate_node() { return O<_Tp_alloc_type>::allocate(_M_impl, 1); }
  _Map_pointer _M_allocate_map(size_t p1) {
    _Map_alloc_type __map_alloc;
    return O<_Map_alloc_type>::allocate(__map_alloc, p1);
  }
  void _M_initialize_map(size_t);
  void _M_create_nodes(_Map_pointer, _Map_pointer);
  enum { _S_initial_map_size = 8 };
  L _M_impl;
};
template <typename _Tp, typename _Alloc>
_Deque_base<_Tp, _Alloc>::~_Deque_base() noexcept {}
template <typename _Tp, typename _Alloc>
void _Deque_base<_Tp, _Alloc>::_M_initialize_map(size_t) {
  size_t __num_nodes(__deque_buf_size(sizeof(_Tp)));
  _M_impl._M_map_size = max((size_t)_S_initial_map_size, 0);
  _M_impl._M_map = _M_allocate_map(_M_impl._M_map_size);
  _Map_pointer __nstart(_M_impl._M_map);
  _Map_pointer __nfinish = __nstart + __num_nodes;
  try {
    _M_create_nodes(__nstart, __nfinish);
  } catch (...) {
  }
  _M_impl._M_start._M_set_node(__nstart);
  _M_impl._M_finish._M_set_node(__nfinish - 1);
  _M_impl._M_start._M_cur = _M_impl._M_start._M_first;
  _M_impl._M_finish._M_cur = _M_impl._M_finish._M_first;
}
template <typename _Tp, typename _Alloc>
void _Deque_base<_Tp, _Alloc>::_M_create_nodes(_Map_pointer __nstart,
                                               _Map_pointer __nfinish) {
  _Map_pointer __cur;
  try {
    for (__cur = __nstart; __cur < __nfinish; ++__cur)
      *__cur = _M_allocate_node();
  } catch (...) {
  }
}
template <typename _Tp, typename _Alloc = allocator<_Tp>>
struct deque : _Deque_base<_Tp, _Alloc> {
  typedef _Deque_base<_Tp, _Alloc> _Base;
  typedef typename _Base::_Map_pointer _Map_pointer;
  typedef _Tp value_type;
  typedef typename _Base::const_iterator const_iterator;
  typedef size_t size_type;
  typedef _Alloc allocator_type;
  using _Base::_M_get_Tp_allocator;
  deque(size_type, value_type __value, allocator_type __a = allocator_type())
      : _Base(__a, 0) {
    _M_fill_initialize(__value);
  }
  const_iterator begin() { return this->_M_impl._M_start; }
  const_iterator end() { return this->_M_impl._M_finish; }
  void _M_fill_initialize(const value_type &);
};
template <typename _Container> auto begin(_Container p1) { return p1.begin(); }
template <typename _Container> auto end(_Container p1) { return p1.end(); }
template <typename _Container> auto cbegin(_Container p1) { return begin(p1); }
template <typename _Container> auto cend(_Container p1) { return end(p1); }
template <typename _Tp, typename _Alloc>
void deque<_Tp, _Alloc>::_M_fill_initialize(const value_type &) {
  _Map_pointer __cur;
  try {
    for (__cur = this->_M_impl._M_start._M_node;
         __cur < this->_M_impl._M_finish._M_node; ++__cur)
      __uninitialized_fill_a(*__cur, *__cur, 0, _M_get_Tp_allocator());
  } catch (...) {
  }
}
template <class> struct char_traits;
namespace __cxx11 {
template <typename _CharT, typename = char_traits<_CharT>,
          typename = allocator<_CharT>>
struct basic_string;
typedef basic_string<char> string;
}
template <> struct char_traits<char> {
  typedef char char_type;
  static int compare(char_type, char_type *p2, size_t p3) {
    return __builtin_memcmp(0, p2, p3);
  }
};
namespace __cxx11 {
template <typename, typename, typename> struct basic_string {
  typedef O<allocator<char>> _Alloc_traits;
  typedef _Alloc_traits::size_type size_type;
  typedef _Alloc_traits::pointer pointer;
  struct _Alloc_hider {
    _Alloc_hider(pointer, allocator<char> && = allocator<char>());
  } _M_dataplus;
  size_type _M_string_length = 0;
  enum { _S_local_capacity = 15 } _M_local_buf[_S_local_capacity];
  basic_string() : _M_dataplus(0) {}
  basic_string(const basic_string &) : _M_dataplus(0) {}
  size_type size() { return _M_string_length; }
  char *data() const { return 0; }
};
//template<> basic_string<char, std::char_traits<char>, std::allocator<char>>::
//_Alloc_hider::_Alloc_hider(char*, std::allocator<char>&&) {}
extern "C" void
_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE12_Alloc_hiderC1EPcOS3_ (...) {}
}
template <typename _CharT>
int operator==(basic_string<_CharT> &p1, const basic_string<_CharT> &p2) {
  return p1.size() && char_traits<_CharT>::compare(0, p2.data(), p1.size());
}
}
struct G {
  template <class Facade> static void increment(Facade p1) { p1.increment(); }
};
template <class Derived> struct H {
  Derived derived() { return *static_cast<Derived *>(this); }
  void operator++() {
    Derived __trans_tmp_1 = derived();
    G::increment(__trans_tmp_1);
  }
};
template <class Derived> struct I { typedef H<Derived> type; };
template <class Derived, class Base> struct M : I<Derived>::type {
  M(Base p1) : m_iterator(p1) {}
  Base base() { return m_iterator; }
  Base &base_reference() { return m_iterator; }
  Base m_iterator;
};
template <class, class> struct N;
template <class Predicate, class Iterator> struct J {
  typedef M<N<Predicate, Iterator>, Iterator> type;
};
template <class Predicate, class Iterator>
struct N : J<Predicate, Iterator>::type {
  typedef typename J<Predicate, Iterator>::type super_t;
  N(Predicate p1, Iterator p2, Iterator p3)
      : super_t(p2), m_predicate(p1), m_end(p3) {}
  void increment() {
    while (this->base() != m_end && !m_predicate(*this->base()))
      ++this->base_reference();
  }
  Predicate m_predicate;
  Iterator m_end;
};
template <class Predicate, class Iterator>
N<Predicate, Iterator> make_filter_iterator(Predicate p1, Iterator p2,
                                            Iterator p3) {
  return N<Predicate, Iterator>(p1, p2, p3);
}
struct Foo {
  std::string bar;
};
int main() {
  std::deque<Foo> foos(0, {});
  std::string test;
  auto p = [test](auto &foo) { return foo.bar == test; };
  auto begin = make_filter_iterator(p, cbegin(foos), cend(foos));
  auto end = make_filter_iterator(p, cend(foos), cend(foos));
  distance(begin, end);
}
