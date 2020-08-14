// PR c++/96199
// { dg-do compile { target c++17 } }

template<int> struct A1 { };
template<int&> struct A2 { };
template<class> struct A3 { };

int i;
template<typename V> struct B {
  enum E { X };
  B(A1<X>, V) { }

  constexpr static V& ir = i;
  B(A2<ir>, V) { }

  B(A3<E>, V) { }
};

// template<class T> B(A1<B<T>::X>,T) -> B<T>;
// template<class T> B(A2<B<T>::ir>,T) -> B<T>;
// template<class T> B(A3<typename B<T>::E>,T) -> B<T>;

int j;
template <> struct B<int> {
  using V = int;

  enum E { X = 1 };
  B(A1<X>, V) { }

  constexpr static V& ir = j;
  B(A2<ir>, V) { }

  B(A3<E>, V) { }
};

B u1 { A1<1>(), 42 };
B u2 { A2<j>(), 42 };
B u3 { A3<B<int>::E>(), 42 };
