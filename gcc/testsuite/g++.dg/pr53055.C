// PR c++/53055
// { dg-do compile }

struct A A :: * p ;
int i = p ->* p ; // { dg-error "" }
