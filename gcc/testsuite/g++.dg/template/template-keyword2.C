// PR c++/94336 - template keyword accepted before destructor names.

template<typename T> void f(T *p) { p->template ~X(); } // { dg-error ".template. keyword not permitted in destructor name" }
template<typename T> struct X {};
void g(X<int> *p) { f(p); }
