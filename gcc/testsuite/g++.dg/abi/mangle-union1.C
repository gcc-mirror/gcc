// PR c++/104847
// { dg-do compile { target c++11 } }
// { dg-additional-options -fabi-compat-version=0 }

struct S { int i; };
union U { S k; };
template <class T, class... Ts> T sink(T&&, Ts&&...);
template <class T>
decltype(sink(U{1},T())) f(T) { return U{1}; }
int main() { f(3); }
// { dg-final { scan-assembler "_Z1fIiEDTcl4sinktl1ULi1EEcvT__EEES1_" } }
