// CWG 1001

template<class T> struct A {
  typedef T arr[3];
};

template<class T> void f(const typename A<T>::arr) { } // #1

template void f<int>(const A<int>::arr);

template <class T> struct B {
  void g(T);
};

template <class T> void B<T>::g(const T) { } // #2
