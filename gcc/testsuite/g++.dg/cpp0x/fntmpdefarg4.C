// PR c++/55724
// { dg-do compile { target c++11 } }

template<int N> struct S {};
template<typename T = int, T N> void f(S<N>) {}
int main() { S<1> s; f(s); }
