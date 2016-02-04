// PR c++/67876 - [6 Regression] ICE when compiling Firefox 38
// Caused by a patch for c/66516 - missing diagnostic on taking
// the address of a builtin function
// { dg-do compile }

template <class T, void (&F)(T*)>
struct S { };

extern void foo (int*);

template <class T, void (&F)(T*)>
void bar (S<T, F>&s) { }

S<int, foo> s;

void foobar (S<int, foo> &s) { bar (s); }
