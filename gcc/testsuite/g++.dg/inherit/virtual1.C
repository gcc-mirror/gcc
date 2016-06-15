// PR c++/27952

struct A
{
    virtual ~A() {}
};

struct B : A, virtual A {};     // { dg-error "duplicate base" }

struct C : A, B {};             // { dg-message "direct base 'A' inaccessible" }

C c;
