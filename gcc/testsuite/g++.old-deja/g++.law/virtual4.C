// GROUPS passed virtual-functions
#include <stdio.h>
#include <stdlib.h>

int aset = 0;
class A
{
  public:
    void Set() { SetProp(); }
    virtual void SetProp() { aset++;}
};

class B:public A
{
  public:
    void SetProp() { if (!aset) { printf ("FAIL\n"); exit (1);} aset--;}
};

int main()
{
    A a;
    B b;
    A *c=new A;
    A *d=new B;

    a.Set();
    b.Set();
    c->Set();
    d->Set();
    printf ("PASS\n");
}
