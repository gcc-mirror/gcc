/* { dg-do compile } */
/* { dg-options "-Wno-sign-compare -Wno-return-type -Wno-overflow" } */
/* { dg-additional-options "-mthumb" { target arm_thumb2_ok } } */

struct A {
  enum { __value };
};
template <class _T1> struct B { _T1 first; };
template <typename _Iterator, bool> struct C {
  typedef typename _Iterator::iterator_type iterator_type;
  static iterator_type _S_base(_Iterator p1) { return p1.base(); }
};
template <typename _RandomAccessIterator>
typename _RandomAccessIterator::difference_type
__distance(_RandomAccessIterator p1, _RandomAccessIterator p2, int) {
  return p2 - p1;
}

template <typename _InputIterator>
typename _InputIterator::difference_type distance(_InputIterator p1,
                                                  _InputIterator p2) {
  return __distance(p1, p2, 0);
}

template <typename _Iterator, typename> class D {
  _Iterator _M_current;

public:
  typedef _Iterator iterator_type;
  typedef int difference_type;
  _Iterator base() { return _M_current; }
};

template <typename _Iterator, typename _Container>
typename D<_Iterator, _Container>::difference_type
operator-(D<_Iterator, _Container> p1, D<_Iterator, _Container> p2) {
  return p1.base() - p2.base();
}

struct F {
  static unsigned short *__copy_m(unsigned short *p1, unsigned short *p2,
                                  unsigned short *p3) {
    int a = p2 - p1;
    if (a)
      __builtin_memmove(p3, p1, a);
    return p3 + a;
  }
};
class G {
public:
  void allocate(int p1) {
    if (p1 > max_size())
      operator new(sizeof(short));
  }
  unsigned max_size() { return -1 / sizeof(short); }
};

template <typename> class L : public G {};

struct H {
  static unsigned short *allocate(int p1) {
    L<short> d;
    d.allocate(p1);
  }
};
struct I {
  template <typename _InputIterator, typename _ForwardIterator>
  static _ForwardIterator __uninit_copy(_InputIterator p1, _InputIterator p2,
                                        _ForwardIterator p3) {
    return copy(p1, p2, p3);
  }
};
struct J {
  typedef unsigned short *pointer;
  struct K {
    unsigned short *_M_start;
    unsigned short *_M_finish;
  };
  J();
  J(int p1, int) { _M_create_storage(p1); }
  K _M_impl;
  pointer _M_allocate(unsigned p1) { p1 ? H::allocate(p1) : pointer(); }
  void _M_create_storage(int p1) { _M_allocate(p1); }
};

C<D<unsigned short *, int>, 1>::iterator_type
__miter_base(D<unsigned short *, int> p1) {
  return C<D<unsigned short *, int>, 1>::_S_base(p1);
}

template <bool, typename _II, typename _OI>
_OI __copy_move_a(_II p1, _II p2, _OI p3) {
  return F::__copy_m(p1, p2, p3);
}

template <bool _IsMove, typename _II, typename _OI>
_OI __copy_move_a2(_II p1, _II p2, _OI p3) {
  return __copy_move_a<_IsMove>(p1, p2, p3);
}

template <typename _II, typename _OI> _OI copy(_II p1, _II p2, _OI p3) {
  C<D<unsigned short *, int>, 1>::iterator_type b, c = __miter_base(p1);
  b = __miter_base(p2);
  return __copy_move_a2<A::__value>(c, b, p3);
}

template <typename _InputIterator, typename _ForwardIterator>
_ForwardIterator uninitialized_copy(_InputIterator p1, _InputIterator p2,
                                    _ForwardIterator p3) {
  return I::__uninit_copy(p1, p2, p3);
}

template <typename _InputIterator, typename _ForwardIterator, typename _Tp>
_ForwardIterator __uninitialized_copy_a(_InputIterator p1, _InputIterator p2,
                                        _ForwardIterator p3, L<_Tp>) {
  return uninitialized_copy(p1, p2, p3);
}

class M : J {
  J _Base;

public:
  M();
  M(int p1, int p2 = int()) : _Base(p1, p2) {}
  M(D<unsigned short *, int> p1, D<unsigned short *, int> p2) {
    _M_initialize_dispatch(p1, p2, int());
  }
  D<pointer, int> begin();
  D<pointer, int> end();
  int size() { return _M_impl._M_finish - _M_impl._M_start; }
  void _M_initialize_dispatch(D<unsigned short *, int> p1,
                              D<unsigned short *, int> p2, int) {
    L<short> e;
    int f = distance(p1, p2);
    _M_impl._M_start = _M_allocate(f);
    _M_impl._M_finish = __uninitialized_copy_a(p1, p2, _M_impl._M_start, e);
  }
};

B<M> g, h;
void twoMeans() {
  M i(g.first.begin(), h.first.end());
  M(i.size());
}
