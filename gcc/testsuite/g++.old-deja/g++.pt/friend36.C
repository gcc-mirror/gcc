// { dg-do assemble  }

template <class T>
void f(T) {} // { dg-error "" } previously defined here

template <class U>
struct S {
  template <class T>
  friend void f(T) {} // { dg-error "" } redeclaration
};

S<int> si;
