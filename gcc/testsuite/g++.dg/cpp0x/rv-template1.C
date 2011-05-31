// PR c++/44870
// { dg-options -std=c++0x }

void foo(int&& data);

template <typename T>
void bar(T t)
{ foo(int()); }

void baz()
{ bar(0); }
