// PR c++/80227
// { dg-do compile { target c++11 } }

template <class T>
int foo (T);

template <class T, class U = T [sizeof (T) - 5]>
int foo (T, U* = 0);

int i = foo (123);
