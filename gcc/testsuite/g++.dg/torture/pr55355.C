/* { dg-do compile } */

struct A
{
    void funcA(void);
};

struct B {};

struct C
{
    void funcC(void) { a_mp->funcA(); }

    char buf_ma[268435456];
    A   *a_mp;
    B    b_m;
};

void
func(C *c_p)
{
    c_p->funcC();
}
