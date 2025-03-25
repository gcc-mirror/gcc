// PR c++/101881
// { dg-do compile { target c++11 } }

template<int N> using A = int __attribute__((vector_size(N)))*;
void foo(A<4>) {}
