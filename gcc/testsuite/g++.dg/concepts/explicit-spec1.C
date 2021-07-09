// PR c++/101098
// { dg-do compile { target concepts } }

template<typename T> concept C = __is_class(T);
struct Y { int n; } y;
template<C T> void g(T) { }
int called;
template<> void g(Y) { called = 3; }
int main() { g(y); }
