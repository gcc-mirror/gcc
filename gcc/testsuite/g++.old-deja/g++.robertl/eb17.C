// { dg-do run  }
#include <stdio.h>

class A {
public:
    virtual void print();
    virtual A * clone();
};

struct Pad {
  virtual void f () {}
};

class B : public Pad, virtual public A {
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
