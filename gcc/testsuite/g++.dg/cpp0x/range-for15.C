// Test for range-based for loop with templates
// and begin/end as member (non-)virtual functions

// { dg-do run }
// { dg-options "-std=c++11" }

unsigned int g;

struct A
{
    virtual int *begin()
    {
        g |= 1;
        return 0;
    }
    int *end()
    {
        g |= 2;
        return 0;
    }
};

struct B : A
{
    virtual int *begin()
    {
        g |= 4;
        return 0;
    }
    int *end()
    {
        g |= 8;
        return 0;
    }
};

extern "C" void abort(void);

int main ()
{
    A a;
    B b;
    A &aa = b;

    g = 0;
    for (int x : a);
    if (g != (1 | 2))
        abort();

    g = 0;
    for (int x : b);
    if (g != (4 | 8))
        abort();

    g = 0;
    for (int x : aa);
    if (g != (4 | 2))
        abort();
}
