// Build don't link:
// GROUPS passed templates
template <class T>
int foo(T t);

int foo<int>(int i) { return 0; } // ERROR - missing template <>

