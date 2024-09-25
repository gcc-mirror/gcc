/* { dg-skip-if "requires hosted libstdc++ for cstdlib malloc" { ! hostedlib } } */

#include <cstdlib>

struct A
{
    virtual int foo (void) 
    {
        return 42;
    }
};

struct B: public A
{
    int *ptr;
    void alloc ()
    {
        ptr = (int*)malloc(sizeof(int));
    }
    int foo (void) 
    { 
        free(ptr); /* { dg-warning "double-'free' of 'b.B::ptr'" } */
        return 0;
    }
};

int test ()
{
    struct B b, *bptr=&b;
    b.alloc ();
    bptr->foo ();  /* { dg-message "\\(6\\) calling 'B::foo' from 'test'" "event 6" } */
    /* { dg-message "\\(9\\) returning to 'test' from 'B::foo'" "event 9" { target *-*-* } .-1 } */
    return bptr->foo ();
}
