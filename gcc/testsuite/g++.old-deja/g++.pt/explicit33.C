// { dg-do assemble  }
// GROUPS passed templates
template <class T>
void foo(T t);

template <>
void foo(int) {}

void foo(int) {}
