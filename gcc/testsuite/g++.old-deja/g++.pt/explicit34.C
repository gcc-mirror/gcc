// Build don't link:
// GROUPS passed templates
template <class T>
void foo(T t);

template <>
void foo(int) {}; // ERROR - redefinition.

template <>
void foo<int>(int) {} // ERROR - redefinition.
