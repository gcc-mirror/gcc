/* { dg-do compile } */

struct A
{
    ~A();
};

void foo();

void bar()
{
    A a;

    foo();
    for (;;)
        foo();
}

/* { dg-final { cleanup-tree-dump "vect" } } */
