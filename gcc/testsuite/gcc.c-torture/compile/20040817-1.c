/* PR 17051: SRA failed to rename the VOPS properly.  */

struct A
{
    char c, d;
};

void foo(struct A *p)
{
    struct A a = *p;

    if (p->c)
        bar1(a);
    else
    {
        if (p) bar2(a,a.c);
        bar3(a.c);
    }
}
