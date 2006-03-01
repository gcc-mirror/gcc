// { dg-do compile }

struct A
{
    double x[4];
};

struct B
{
    A y[2];
};

A foo(B *p)
{
    for ( int i=0; i<4; ++i )
        p->y[1].x[i]=0;

    A a;
    return a;
}
