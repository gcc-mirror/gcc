// { dg-do compile }

template <typename C> struct A
{
  typedef typename C::iterator type;
};
template <typename T2> struct B
{
  typedef T2 type;
};
template <typename F2> struct L
{
  typedef typename B<F2>::type::type type;
};
template <typename C> struct M
{
  typedef typename L<A<C> >::type type;
};
class C
{
public:
  typedef int iterator;
};
template <class IteratorT> class D
{
public:
  typedef IteratorT iterator;
  template <class Iterator> D (Iterator p1, Iterator) : m_Begin (p1), m_End (0)
  {
  }
  IteratorT m_Begin;
  IteratorT m_End;
};
template <class IteratorT> class I : public D<IteratorT>
{
protected:
  template <class Iterator>
  I (Iterator p1, Iterator p2)
      : D<IteratorT> (p1, p2)
  {
  }
};
class F
{
public:
  int nelems;
  int elems[];   // { dg-error "not at end" }
  int *
  m_fn1 ()
  {
    return elems;
  }
};
class G
{
public:
  void *
  m_fn2 (int)
  {
    return m_buffer.m_fn1 ();
  }
  F m_buffer;
};
struct any_incrementable_iterator_interface
{
  virtual ~any_incrementable_iterator_interface () {}
};
class J : public any_incrementable_iterator_interface
{
public:
  J (int) : m_it () {}
  int m_it;
};
void *operator new(__SIZE_TYPE__, void *p2) { return p2; }
template <class T> typename M<T>::type begin (T) { return 0; }
template <class T> typename M<T>::type end (T) {}
template <class> class any_iterator
{
public:
  template <class WrappedIterator> any_iterator (WrappedIterator)
  {
    void *ptr = m_buffer.m_fn2 (0);
    m_impl = new (ptr) J (0);
  }
  ~any_iterator ()
  {
    if (m_impl)
      m_impl->~any_incrementable_iterator_interface ();
  }
  G m_buffer;
  any_incrementable_iterator_interface *m_impl;   // { dg-message "next member" }
};
template <class Reference> class K : public I<any_iterator<Reference> >
{
public:
  template <class WrappedRange>
  K (WrappedRange p1)
      : I<any_iterator<Reference> > (begin (p1), end (p1))
  {
  }
};
template <class Reference> struct H
{
  typedef K<Reference> type;
};
template <class, class, class, class, class, class TargetReference>
void
mix_values_impl ()
{
  C test_data;
  H<int>::type source_data (test_data);
  typename H<TargetReference>::type t2 = source_data;
}
template <class>
void
mix_values_driver ()
{
  mix_values_impl<int, int, int, int, int, int &> ();
}
void
mix_values ()
{
  mix_values_driver<int> ();
}
