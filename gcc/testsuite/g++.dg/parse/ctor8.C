//PR c++/28505

struct A
{
    A : ();     // { dg-error "primary-expression|incomplete type" }
    A : (int);  // { dg-error "primary-expression|incomplete type|'int'" }
};

A a = (A){0};   // { dg-error "too many initializers|compound-literals" }
