// Build don't link:

template <class T> 
struct S1 {
  friend bool f<>(const S1&);
  typedef T X;
};

template <class T>
struct S2 {
};

template <class T>
struct S2<S1<T> > {
  typedef typename S1<T>::X Y;
};

template <class T>
bool f(T);

template <class T>
typename S2<S1<T> >::Y
f(const S1<T>&);

template struct S1<int>;
