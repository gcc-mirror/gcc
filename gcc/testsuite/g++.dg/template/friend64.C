template <class T> void foo (int);

template <class T>
class Q {
  friend void foo<T> (int) { }  // { dg-error "15:defining explicit specialization" }
};
