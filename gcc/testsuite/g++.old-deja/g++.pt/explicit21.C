// { dg-do assemble  }
// GROUPS passed templates
template <class T>
T foo(T* t);

template <>
int foo<char>(char c); // { dg-error "" } does not match declaration.

template <>
int bar<char>(); // { dg-error "" } no template bar.
