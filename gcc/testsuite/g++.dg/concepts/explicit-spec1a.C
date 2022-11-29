// A version of explicit-spec1.C where the template g has trailing instead of
// template requirements.
// PR c++/107864
// { dg-do compile { target concepts } }

template<typename T> concept C = __is_class(T);
struct Y { int n; } y;
template<class T> void g(T) requires C<T> { }
int called;
template<> void g(Y) { called = 3; }
int main() { g(y); }
