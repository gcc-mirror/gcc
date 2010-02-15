/* { dg-do compile } */
/* { dg-options "-freorder-blocks -ftracer} */

struct A {
    virtual A *f();
};
struct B : virtual A {
    virtual B *f();
};
B *B::f() { return 0; }
