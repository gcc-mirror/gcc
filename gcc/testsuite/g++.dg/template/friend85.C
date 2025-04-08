// PR c++/119378

template<int N>
struct A {
  template<class T>
  struct B;
};

template<class U>
struct C {
  template<int N>
  template<class T>
  friend class A<N>::B;
};

template struct C<int>;
