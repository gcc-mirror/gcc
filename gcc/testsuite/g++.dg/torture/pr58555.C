/* { dg-do compile } */
template <typename _Tp> _Tp *__addressof(_Tp &) {}
template <typename _Tp> class A {
public:
  typedef _Tp *pointer;
};
template <typename _Tp> class M : public A<_Tp> {
public:
  typedef M other;
  ~M();
};
class B {
public:
  B(int *);
};
class C {
public:
  void GetNext();
  C *GetChildren();
};
template <typename _Tp> void _Destroy(_Tp *p1) { p1->~_Tp(); }
struct D {
  template <typename _ForwardIterator>
  static void __destroy(_ForwardIterator p1, _ForwardIterator p2) {
    for (; p1 != p2; ++p1)
      _Destroy(__addressof(*p1));
  }
};
template <typename _ForwardIterator>
void _Destroy(_ForwardIterator p1, _ForwardIterator p2) {
  D::__destroy(p1, p2);
}
template <typename _ForwardIterator, typename _Tp>
void _Destroy(_ForwardIterator p1, _ForwardIterator p2, M<_Tp> &) {
  _Destroy(p1, p2);
}
template <typename _Alloc> struct F {
  typedef _Alloc _Tp_alloc_type;
  typedef typename _Tp_alloc_type::pointer pointer;
  struct N : _Tp_alloc_type {
    pointer _M_start;
    pointer _M_finish;
  };
  _Tp_alloc_type &_M_get_Tp_allocator();
  N _M_impl;
};
template <typename _Tp, typename _Alloc = M<_Tp> > class O : F<_Alloc> {
using  F<_Alloc>::_M_get_Tp_allocator;
public:
  ~O() {
    _Destroy(this->_M_impl._M_start, this->_M_impl._M_finish,
             _M_get_Tp_allocator());
  }
};
template <class T> void checked_delete(T *p1) { delete p1; }
template <class> class I;
template <class T> struct J {
  typedef T *type;
};
class K;
class L {
public:
  virtual ~L();
};
class P : L {
  O<I<int> > databasesM;
  O<I<K> > usersM;
public:
  I<int> addDatabase();
};
C a;
C *b;
int atomic_exchange_and_add();
class G {
public:
  virtual void dispose() = 0;
  void release() {
    if (atomic_exchange_and_add() == 1)
      dispose();
  }
};
class Q : G {
  P *px_;
  Q() {}
  void dispose() { checked_delete(px_); }
};
class H {
  G *pi_;
public:
  H();
  H(P *);
  ~H() {
    if (pi_)
      pi_->release();
  }
};
template <class T, class Y> void sp_pointer_construct(I<T> *, Y, H);
template <class T> class I {
public:
  typedef T element_type;
  template <class Y> I(Y *p1) { sp_pointer_construct(this, 0, 0); }
  typename J<T>::type operator->();
  H pn;
};
void getNodeContent(const B &) {
  for (C *n = a.GetChildren(); n; n->GetNext())
    ;
}
void parseDatabase(I<P> p1) {
  I<int> c = p1->addDatabase();
  for (; b;)
    getNodeContent(0);
}
void addServer() { I<int>(new P); }
