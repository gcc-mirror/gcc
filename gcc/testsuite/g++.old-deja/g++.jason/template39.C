// PRMS Id: 10283
// Build don't link:

template <class T> struct B {
  static void (*p)();
  static void f ();
};

template <class T>
void (*B<T>::p)() = &B<T>::f;

B<int> b;

template <int i> struct A {
  static const int j = i;
  int k[j];
};

A<1> a;

template <int i>
const int A<i>::j;
