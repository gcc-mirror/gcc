// { dg-do assemble  }
struct S {
  template <class T>
  int f(T), g(T); // { dg-error "" } more than one declarator
};

template <class T>
void x(T), y(T); // { dg-error "" } more than one declarator

template <class T>
struct S2 
{
  static int i, j; // OK.
};

template <class T>
int S2<T>::i, S2<T>::j; // { dg-error "" } more than one declarator

template <>
int S2<int>::i, S2<double>::i;  // { dg-error "" } more than one declarator
