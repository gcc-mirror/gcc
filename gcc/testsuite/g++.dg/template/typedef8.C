// PR c++/34206

template<class _T1, class _T2> struct pair { };
template <class T0, class T1> struct tuple {
  template <class U1, class U2>
  tuple& operator=(const pair<U1, U2>& k) { }
};
template<class T1, class T2> inline tuple<T1&, T2&> tie(T1& t1, T2& t2) { }

template <class T> struct A
{   
  typedef T type;
  pair<type, type> f();
};

void g(A<int> a)
{
  typedef A<int>::type type;
  type begin1, end1;
  tie(begin1, end1) = a.f();
}
