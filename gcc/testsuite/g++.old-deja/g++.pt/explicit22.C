// Build don't link:
// GROUPS passed templates
template <class T, class U>
T foo(T t, U* u);

template <class T>
T foo(T t, T* t);

template <>
int foo<int>(int, int*); // ERROR - ambiguous specialization.
