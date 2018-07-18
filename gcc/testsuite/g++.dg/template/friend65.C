template <class T> void foo (int);

template <class T>
class Q {
  friend inline void foo<T> (int); // { dg-error "10:.inline. is not allowed in declaration of friend" }
};
