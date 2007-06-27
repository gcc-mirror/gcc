/* This used to ICE (PR c++/27492) */
/* { dg-do "compile" } */

struct A {};

class B : A
{
    virtual A* foo(); /* { dg-error "overriding" } */
};

struct C : virtual B
{
    virtual C* foo(); /* { dg-error "invalid covariant return type" } */
};

C* C::foo() { return 0; }

struct D : C {};
