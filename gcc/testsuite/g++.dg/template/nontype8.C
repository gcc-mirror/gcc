// { dg-do compile }
// Origin: C++ standard, [temp.arg.nontype]/3

template<int* p> class X { };

int a[10];
struct S { int m; static int s; } s;

X<&a[2]> x3;                    // { dg-error "3:.& a\\\[2\\\]. is not a valid template argument" "" { target c++17 } }
// { dg-error "" "" { target c++14_down } .-1 }
X<&s.m> x4;                     // { dg-error "3:.& s.S::m. is not a valid template argument" "" { target c++17 } }
// { dg-error "" "" { target c++14_down } .-1 }
X<&s.s> x5;                     // { dg-error "" "" { target { ! c++17 } } } &S::s must be used
X<&S::s> x6;                    // OK: address of static member

