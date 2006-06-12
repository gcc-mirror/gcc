// { dg-do compile }

template <class T> void foo (int);

template <class T>
class Q {
  friend void foo<T> (int = 3); // { dg-error "default argument" }
};
