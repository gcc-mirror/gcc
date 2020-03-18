// DR 314 - template in base class specifier.

template <typename T>
struct A {
  template <typename U>
  struct B {};
};

template <typename T>
struct C : public A<T>::template B<T> {
};

template <typename T>
struct C2 : public A<int>::B<T> {
};
