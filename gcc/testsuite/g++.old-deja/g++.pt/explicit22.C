// { dg-do assemble  }
// GROUPS passed templates
template <class T, class U>
T foo(T t, U* u);

template <class T>
T foo(T t, T* u);

template <>
int foo<int>(int, int*);
