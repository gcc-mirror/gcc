// PR c++/67407

template <class> class A;
template <class> struct B;
template <class X> struct B<A<X> >
{
  static int
  check ()
  {
    A<X> a;
    a.m_class->m_object;
  }
};
template <class T> class A
{
public:
  template <class X> bool operator== (const X &) const;
  T *m_class;
};
template <class T>
template <class X>
bool
A<T>::operator== (const X &) const
{
  B<X>::check;
}
class C
{
protected:
  template <class> friend struct B;
  void *m_object;
};
class F : virtual C
{
};
class G : virtual public C
{
};
class H : F, public G
{
};
class D
{
  void onBusMessage (const A<int> &);
  A<H> m_pipeline;
};
void
D::onBusMessage (const A<int> &p1)
{
  p1 == m_pipeline;
}
