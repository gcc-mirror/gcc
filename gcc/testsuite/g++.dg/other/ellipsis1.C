// PR c++/26291
// { dg-do compile }

struct A
{
    A& operator= (A,...);  // { dg-error "variable number of arguments" }
    A operator+ (...);     // { dg-error "variable number of arguments" }
    operator int(...);     // { dg-error "variable number of arguments" }
    int operator() (...);
};

A operator- (A,...);       // { dg-error "variable number of arguments" }
