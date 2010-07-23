/* PR 19952 */
/* { dg-do compile } */
/* { dg-options "-ftree-vectorize -O2" } */

int i;

struct A
{
    ~A() { ++i; }
};

struct B
{
    A a;
};

void foo()
{
    for (int i=0; i<2; ++i)
    {
        B *p;
        if (p) p->~B();
    }
}
