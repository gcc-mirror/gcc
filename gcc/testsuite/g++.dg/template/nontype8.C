// { dg-do compile }
// Origin: C++ standard, [temp.arg.nontype]/3

template<int* p> class X { };

int a[10];
struct S { int m; static int s; } s;

X<&a[2]> x3;                    // { dg-error "" } address of array element
X<&s.m> x4;                     // { dg-error "" } address of non-static member
X<&s.s> x5;                     // { dg-error "" "" { target { ! c++17 } } } &S::s must be used
X<&S::s> x6;                    // OK: address of static member

