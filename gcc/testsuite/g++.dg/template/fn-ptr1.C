// PR c++/48162

struct A { };
A (*f)();
template <class T> void g() { f(); }
