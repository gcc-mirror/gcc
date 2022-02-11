/*
TEST_OUTPUT:
---
fail_compilation/fail94.d(28): Error: cannot implicitly override base class method `fail94.A.clone` with `fail94.B.clone`; add `override` attribute
---
*/
interface I
{
    int foo();
}

class IA : I
{
    int foo() { return 1; }
}

class A
{
    I i;

    I clone() { return i; }
}

class B : A
{
    IA ia;

    IA clone()
    out (result)
    {
	printf("B.clone()\n");
    }
    do { return ia; }
}

void main()
{
    IA ia = new IA;
    assert(ia.foo() == 1);

    I i = ia;
    assert(i.foo() == 1);

    A a = new A;
    a.i = i;
    assert(a.clone().foo() == 1);

    B b = new B;
    b.ia = ia;
    assert(b.clone().foo() == 1);

    a = b;
    assert(a.clone().foo() == 1);

    bar(&b.clone);
}


void bar(IA delegate() dg)
{
}
