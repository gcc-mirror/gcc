// { dg-do assemble  }
// Test for use of template parms in constructor name.
// Submitted by Jason Merrill <jason@cygnus.com>

template <class T>
struct A {
  A<T>();
};

template <class T>
A<T>::A<T>()   // { dg-error "invalid use of constructor|qualified name" }
{
}
