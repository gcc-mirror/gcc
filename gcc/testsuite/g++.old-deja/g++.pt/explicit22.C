// Build don't link:
// GROUPS passed templates
template <class T, class U>
T foo(T t, U* u); // ERROR - template candidate

template <class T>
T foo(T t, T* t); // ERROR - template candidate

template <>
int foo<int>(int, int*); // ERROR - ambiguous template specialization
