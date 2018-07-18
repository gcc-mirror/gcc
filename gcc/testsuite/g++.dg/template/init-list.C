// Contributed by Gabriel Dos Reis <gdr@codesourcery.com>
// { dg-do compile }

template<typename T>
struct Base {
  Base(int) { }
};

template<typename T>
struct Derived : Base<T> {
  Derived();
};

template<typename T>
Derived<T>::Derived() : Base(4) { } // { dg-error "have any field" }


