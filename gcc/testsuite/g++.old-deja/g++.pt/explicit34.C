// { dg-do assemble  }
// GROUPS passed templates
template <class T>
void foo(T t);

template <>
void foo(int) {}; // { dg-error "" } previously defined here.

template <>
void foo<int>(int) {} // { dg-error "" } duplicate specialization.
