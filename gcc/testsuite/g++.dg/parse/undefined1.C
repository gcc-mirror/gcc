// PR c++/8143
// { dg-do compile }

struct foo
{
    X x;                   // { dg-error "" }

    foo(X)              {} // { dg-error "" }
    foo(X y, int) : x() {} // { dg-error "" }
};
