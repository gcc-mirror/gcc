// Build don't link:

template <class T> struct A {};
template <class T> struct A<T*>;
A<int*> ai; // ERROR - incomplete type
