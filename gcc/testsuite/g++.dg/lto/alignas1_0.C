// PR c++/104245
// { dg-lto-do assemble }
// { dg-require-effective-target c++11 }

template <typename T> struct A { alignas(T) alignas(int) int a; };
struct B { B(const char *, const char *, int, int); A<int> b; };
B c {"", "", 0, 0};
