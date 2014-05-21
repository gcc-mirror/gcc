// Origin PR c++/51477
// { dg-do compile { target c++11 } }

struct A
{
    typedef int int T; // { dg-error "two or more data types in declaration" }
    struct T x[1] = { 0 }; // { dg-error "incomplete type|forward" }
};
