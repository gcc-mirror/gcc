// Build don't link:
// GROUPS passed templates
template <class T>
T foo(T t);

template <>
int foo<char>(char c); // ERROR - does not match any template declaration
