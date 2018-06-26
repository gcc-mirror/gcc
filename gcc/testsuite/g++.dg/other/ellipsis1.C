// PR c++/26291
// { dg-do compile }

struct A
{
    A& operator= (A,...);  // { dg-error "8:.A& A::operator=\\(A, ...\\). must not have variable number of arguments" }
    A operator+ (...);     // { dg-error "7:.A A::operator\\+\\(...\\). must not have variable number of arguments" }
    operator int(...);     // { dg-error "5:.A::operator int\\(...\\). must not have variable number of arguments" }
    int operator() (...);
};

A operator- (A,...);       // { dg-error "3:.A operator-\\(A, ...\\). must not have variable number of arguments" }
