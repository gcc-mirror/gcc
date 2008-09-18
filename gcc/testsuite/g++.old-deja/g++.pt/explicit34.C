// { dg-do assemble  }
// { dg-options "-fshow-column" }
// GROUPS passed templates
template <class T>
void foo(T t);

template <>
void foo(int) {}; // { dg-error "6:previously declared here" }

template <>
void foo<int>(int) {} // { dg-error "6:redefinition" }
