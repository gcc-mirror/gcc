// PR c++/23055

template <class> struct S { typedef int type; };

template <class T>
int foo(T, typename S<T>::type * ret);

int j = foo(1, 0);
