// Origin: Volker Reichelt <reichelt@igpm.rwth-aachen.de>
// PR c++/18731

struct A
{
    struct B;
    typedef B C;
};

struct A::C {}; // { dg-error "invalid class name" }
