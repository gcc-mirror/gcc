// PR c++/7917
// Origin: VACLAV HAISMAN <V.Haisman@sh.cvut.cz>
// { dg-do compile }

struct A
{
    int i;
    void foo() = 0 {} // { dg-error "" }

    A() : i(0) {}
};
