template <class T> struct A {
  static const bool b = false;
};

template <class T>
const bool A<T>::b;

void f ();
