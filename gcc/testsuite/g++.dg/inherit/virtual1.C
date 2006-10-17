//PR c++/27952

struct A
{
    virtual ~A() {}
};

struct B : A, virtual A {};     // { dg-error "duplicate base|forward declaration" }

struct C : A, B {};             // { dg-error "duplicate base|invalid use" }

C c;                            // { dg-error "aggregate" }
