// Build don't link:

template <class T>
struct C {
  template <class U>
  void f(U); // OK

  template <class V = int>
  struct I {}; // OK

  template <class W = int>
  void h(W); // ERROR - default argument
  
  template <class Y>
  void k(Y);
};

template <class T>
template <class U = double>
void C<T>::f(U) {} // ERROR - default argument

template <class X = void*>
void g(X); // ERROR - default argument

template <class T = double>
template <class Y>
void C<T>::k(Y) {} // ERROR - default argument
