// Build don't link:

template <class T>
void f(T) {} // ERROR - previously defined here

template <class U>
struct S {
  template <class T>
  friend void f(T) {} // ERROR - redeclaration
};

S<int> si;
