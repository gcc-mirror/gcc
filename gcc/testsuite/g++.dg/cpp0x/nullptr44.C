// PR c++/91979
// { dg-do compile { target c++11 } }
// { dg-final { scan-assembler "_Z3fooILPv0EEvPN9enable_ifIXeqT_LDnEEvE4typeE" } }

template <bool, typename T = void>
struct enable_if {};

template <typename T>
struct enable_if<true, T> { typedef T type; };

template <void *P>
void foo(typename enable_if<P == nullptr>::type* = 0) {}

template void foo<(void *)0>(void *);

