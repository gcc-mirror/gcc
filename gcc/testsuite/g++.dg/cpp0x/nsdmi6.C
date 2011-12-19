// Origin PR c++/51477
// { dg-options "-std=c++11" }

struct A
{
    typedef int int T; // { dg-error "two or more data types in declaration" }
    struct T x[1] = { 0 }; // { dg-error "invalid|forward" }
};
