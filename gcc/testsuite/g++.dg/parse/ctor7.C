//PR c++/28505

struct A
{
    A : ();     // { dg-error "primary-expression|incomplete type" }
    A : (int);  // { dg-error "primary-expression|incomplete type|'int'" }
};

struct B
{
    char c;
    A a;
};

B b = (B){0};   // { dg-error "compound-literals" }
