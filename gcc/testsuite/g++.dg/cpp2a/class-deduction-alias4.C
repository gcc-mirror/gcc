// PR c++/96199
// { dg-do compile { target c++2a } }

template<int> struct A1 { };
template<int&> struct A2 { };
template<class> struct A3 { };

int i;
template<typename V, int> struct B {
  enum E { X };
  B(A1<X>, V) { }

  constexpr static V& ir = i;
  B(A2<ir>, V) { }

  B(A3<E>, V);
};

// template<class T, int I> B(A1<B<T,I>::X>,T) -> B<T,I>;
// template<class T, int I> B(A2<B<T,I>::ir>,T) -> B<T,I>;
// template<class T, int I> B(A3<typename B<T,I>::E>,T) -> B<T,I>;

template <typename T> using U = B<T, 1>;

// template<class T> B(A1<B<T,1>::X>,T) -> B<T,1>;
// template<class T> B(A2<B<T,1>::ir>,T) -> B<T,1>;
// template<class T> B(A3<typename B<T,1>::E>,T) -> B<T,1>;

int j;
template <> struct B<int, 1> {
  using V = int;

  enum E { X = 1 };
  B(A1<X>, V) { }

  constexpr static V& ir = j;
  B(A2<ir>, V) { }

  B(A3<E>, V);
};

U u1 { A1<1>(), 42 };
U u2 { A2<j>(), 42 };
U u3 { A3<U<int>::E>(), 42 };
