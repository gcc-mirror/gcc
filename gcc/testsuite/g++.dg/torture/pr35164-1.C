/* { dg-additional-options "-Wno-return-type" } */

typedef __SIZE_TYPE__ size_t;
template<typename _Iterator, typename _Container> class __normal_iterator {
public:
  const _Iterator& base() const;
};
template<typename _BI1, typename _BI2> inline
void copy_backward(_BI1 __first, _BI1 __last, _BI2 __result) {
  while (__first != __last) *--__result = *--__last;
}
template<typename _Tp> struct _Vector_base {
  struct _Vector_impl { _Tp* _M_finish; };
  _Vector_impl _M_impl;
};
template<typename _Tp > class vector : protected _Vector_base<_Tp> {
  typedef vector<_Tp> vector_type;
  typedef _Tp * pointer;
  typedef _Tp & reference;
  typedef __normal_iterator<pointer, vector_type> iterator;
  typedef size_t size_type;
public:
  iterator end();
  void resize(size_type __new_size) { insert(end(), __new_size); }
  reference operator[](size_type __n);
  void insert(iterator __position, size_type __n)
  {
    pointer __old_finish(this->_M_impl._M_finish);
    copy_backward(__position.base(), __old_finish - __n, __old_finish);
  }
};
struct A {
  virtual ~A ();
  void incRef ();
  void decRef ();
};
struct C : public A {
  static C *alloc ();
};
template <class T> struct B {
  B () : ptr (T::alloc ()) { }
  B (T *a_ptr) : ptr (a_ptr) { }
  ~B () { decRef (); }
  B& operator= (const B<T>& a) { if (a.get () != this->get ()) { decRef ();
incRef (); } }
  template<class U> operator B<U> () const { return B<U> (ptr); }
  T* operator-> () const { }
  T* get () const { return ptr; }
  void decRef () const { if (ptr != 0) ptr->decRef (); }
  void incRef () const { if (ptr != 0) ptr->incRef (); }
  T *ptr;
};
struct D : public C {
  template <class T> inline void foo (const B<T> & x) { d.resize (1); d[0] = x;
}
  vector<B <C> > d;
};
struct E : public C {
  static E *alloc ();
};
struct F : public D {
  static F *alloc ();
};
void foo (vector<B<D> > & x) {
  for (int i = 0; i < 2; ++i)
    {
      B<F> l;
      B<E> m;
      l->foo (m);
    }
}
