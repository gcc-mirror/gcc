// { dg-do assemble  }

template <class T> struct A {
  static void f();
};

template <class T> class B
{
  friend class A<T>;
  static int i; // { dg-error "" } private
};

template <class T> class C
{
  template <class U>
  friend class A;

  static int i;
};

template <class T>
void A<T>::f()
{
  B<T>::i = 3;
  C<T>::i = 3;
  C<double>::i = 3;
  B<double>::i = 3; // { dg-error "" } member `i' is private
}

template void A<int>::f();
