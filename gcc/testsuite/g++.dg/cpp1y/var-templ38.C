// PR c++/66596
// { dg-do compile { target c++14 } }

struct U { void f() {} };
struct V { void f() {} };
template<class T> U t;
template<> V t<int>;
template<class T> void g() { t<T>.f(); }
int main() { g<int>(); }
