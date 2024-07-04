// { dg-do assemble  }
// Test for use of template parms in constructor name.
// Submitted by Jason Merrill <jason@cygnus.com>

template <class T>
struct A {
  A<T>(); // { dg-error "template-id" "" { target c++20 } }
};

template <class T>
A<T>::A<T>()   // { dg-error "constructor|qualified name" }
{
}
