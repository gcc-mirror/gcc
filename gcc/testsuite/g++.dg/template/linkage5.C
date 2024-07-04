// PR c++/115283
// { dg-final { scan-assembler "(weak|glob)\[^\n\]*_Z1fIiEv1AIX1CIT_EEE" } }
// { dg-do compile { target c++20 } }

template<class T>
concept C = true;

template<bool B>
struct A { };

template<class T>
void f(A<C<T>>) { }

template void f<int>(A<true>);
