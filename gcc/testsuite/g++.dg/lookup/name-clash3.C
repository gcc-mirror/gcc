// { dg-do compile }

// Origin: Volker Reichelt <reichelt@igpm.rwth-aachen.de>
// PR c++/12102

struct A
{
    struct C {} C;
};

struct B : A
{
    struct C {} C;
};
