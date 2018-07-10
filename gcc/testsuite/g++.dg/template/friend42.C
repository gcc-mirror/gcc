// { dg-do compile }

template <class T> void foo (int);

template <class T>
class Q {
  friend void foo<T> (int = 3); // { dg-error "27:default arguments are not allowed in declaration of friend" }
};
