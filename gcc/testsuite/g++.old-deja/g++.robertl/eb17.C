// excess errors test - XFAIL
// covariant return types in are currently not support for complex inheritance
#include <stdio.h>

class A {
public:
    virtual void print();
    virtual A * clone();
};

class B : virtual public A {
public:
    void print();
    B * clone();
};

void A::print()
{
    printf("A\n");
}

void B::print()
{
    printf("B\n");
}


A * A::clone()
{
    return this;
}

B * B::clone()
{
    return this;
}


int main()
{
    A * a = new B;
    B * b = dynamic_cast<B *>(a);

    printf("%p\n",b);                // (*2*)
    b->print();

    a = b;
    printf("%p\n",a);
    a->print();

    a = a->clone();
    printf("%p\n",a);
    a->print();                      // (*1*)

    return 0;
}
