// Build don't link:

template <class T> struct A {};
template <class T> struct A<T>;    // ERROR - does not specialize args
template <class T> const struct A; // ERROR - parse error
template <class T> template A<int>; // ERROR - .*
