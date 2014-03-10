// PR c++/44870
// { dg-do compile { target c++11 } }

void foo(int&& data);

template <typename T>
void bar(T t)
{ foo(int()); }

void baz()
{ bar(0); }
