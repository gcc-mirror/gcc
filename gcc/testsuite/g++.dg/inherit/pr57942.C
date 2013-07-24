// PR c++/57942

template<typename T> struct S { typename T::error type; };
struct X {};
void f(S<int>*);
void f(...);
void g() { f((X*)0); }
struct Y;
void h() { f((Y*)0); }
