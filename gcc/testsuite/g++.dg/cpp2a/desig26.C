// PR c++/105925
// { dg-do compile { target c++20 } }

struct V
{
    int i;
    double d;
};

struct X
{
    union
    {
        int x;
        V y;
    };
};

X foo()
{
    return {.y = {0, 0.0}};
}
