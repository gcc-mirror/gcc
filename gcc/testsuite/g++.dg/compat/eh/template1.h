class A {};

template <class T>
struct B
{
  typedef A E;
};

template <class T>
struct C
{
  typedef B<T> D;
  typedef typename D::E E;
  void f()
#if __cplusplus < 201103L
  throw(E)
#endif
  ;
};
