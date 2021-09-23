#include <cstdio>
#include <cstdlib>

struct A
{
    int m_data;
    A() {m_data = 0;}
    virtual int deallocate (void) 
    {
        return 42;
    }
};

struct B: public A
{
    int *ptr;
    int m_data_b;
    B() {m_data_b = 0;}
    void allocate ()
    {
        ptr = (int*)malloc(sizeof(int));
    }
    int deallocate (void) 
    { 
        free(ptr);
        return 0;
    }
};

void foo(A *a_ptr)
{
    printf("%d\n",a_ptr->deallocate());
}

void test()
{
    B b;
    A a, *aptr;
    aptr = &b;
    b.allocate();
    foo(aptr);
    aptr = &a;
    foo(aptr);
}
