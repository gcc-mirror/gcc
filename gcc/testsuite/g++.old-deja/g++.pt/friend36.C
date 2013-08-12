// { dg-do assemble  }

template <class T>
void f(T) {} // { dg-message "previously" }

template <class U>
struct S {
  template <class T>
  friend void f(T) {} // { dg-error "redefinition" }
};

S<int> si;
