template <class> class A
{
  void m_fn1 (int *, int);
};

template <class> class B
{
public:
  typedef int Type;
};

template <class> class C
{
public:
  C (int);
  template <template <class> class T> void m_fn2 (typename T<void>::Type);
};

template <>
void
A<int>::m_fn1 (int *, int)
{
  C<int> a (0);
  a.m_fn2<B> (0);
}
