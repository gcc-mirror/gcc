// Test for use of template parms in constructor name.
// Submitted by Jason Merrill <jason@cygnus.com>
// Build don't link:

template <class T>
struct A {
  A<T>();
};

template <class T>
A<T>::A<T>()
{
}
