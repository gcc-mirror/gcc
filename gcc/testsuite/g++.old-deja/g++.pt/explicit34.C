// Build don't link:
// GROUPS passed templates
template <class T>
void foo(T t);

template <>
void foo(int) {}; 

template <>
void foo<int>(int) {} // ERROR - duplicate specialization.
