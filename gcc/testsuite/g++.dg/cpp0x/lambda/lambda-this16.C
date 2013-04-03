// PR c++/56699
// { dg-require-effective-target c++11 }

struct A
{
    int a;
};

struct T
{
    int x;

    T() : x([]{
        sizeof(::A::a);
        return 0;
    }())
    {}
};

struct B
{
    int a;
};

void f()
{
    []{sizeof(B::a);};
}
