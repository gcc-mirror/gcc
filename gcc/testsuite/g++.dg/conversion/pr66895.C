// PR c++/66895
// { dg-do compile { target c++11 } }

#include <cstddef>
#include <initializer_list>

struct S {
    template<std::size_t N> S(char const (&)[N]);
};
struct T1 { S s; };
void f1(std::initializer_list<T1>);
void g1() { f1({{""}}); }

struct T2 { const S& s; };
void f2(std::initializer_list<T2>);
void g2() { f2({{""}}); }
