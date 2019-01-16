
extern(C) int printf(const char*, ...);

/*******************************************/

interface D {
        int foo();
}

class A : D {
        int foo() { return 1; }
}

class B : A, D
{
        override int foo() { return 2; }
}

class C : B, D {
        override int foo() { return 3; }
}

void test1()
{
        C c = new C;
        A a = cast(A) c;
        int j = a.foo();
        printf("%d\n", j);
        assert(j == 3);

        B b = cast(B) c;
        int k = b.foo();
        printf("%d\n", k);
        assert(k == 3);

        D d1 = cast(D) c;
        int l = d1.foo();
        printf("%d\n", l);
        assert(l == 3);

        D d2 = cast(D) b;
        int m = d2.foo();
        printf("%d\n", m);
        assert(m == 3);
}


/*******************************************/

interface D2 {
        int foo();
}

class A2 : D2 {
        int foo() { printf("A2\n"); return 1; }
}

class B2 : A2 {
        override int foo() { printf("B2\n"); return 2; }
}

class C2 : B2, D2 {
        override int foo() { printf("C2\n"); return 3; }
}

void test2()
{
    int i;

    C2 c = new C2;
    D2 d = cast(D2)c;
    i = c.foo();
    assert(i == 3);

    i = d.foo();
    assert(i == 3);

    B2 b = new B2;
    if (cast(D2) b)
    {
        D2 e = cast(D2) b;
        i = e.foo();
        assert(i == 2);
    }
    else
        assert(0);

    A2 a;
    if (cast(D2) a)
        assert(0);
}


/*******************************************/

interface C3
{
    int doLayout();
}

class A3
{
    void print() { printf("A3::print\n"); }
}

class B3 : A3, C3
{
    int doLayout() { printf( "B3::doLayout\n" ); return 17; }
}

void callLayout(A3 b)
{
    printf("b = %p\n", b);
    C3 cl = cast(C3)b;
    printf("cl = %p\n", cl);
    if (cl)
    {   int i;

        i = cl.doLayout();
        assert(i == 17);
    }
    else
    {   printf("the 'A3' you passed did not implement 'C3'\n" );
        assert(0);
    }
}

void test3()
{
    callLayout(new B3());
}


/*******************************************/


template IContainer(T)
{
        interface IContainer
        {
                alias   Container!(int) selected_type;
                bool    isEmpty();
                int     enumerate();
        }
}

template Container(T)
{
        class Container : IContainer!(int)
        {
            bool isEmpty() { return false; }
            int enumerate() { return 3; }
        }
}

void Vector_test_IContainer_int()
{
    alias IContainer!(int) icontainer_t;
}

void test4()
{
}

/*******************************************/


interface Italy(T)
{
        alias   Foo5!(int) selected_type;
        bool    isempty();
        int     enumerate();
}

class Foo5(T) : Italy!(int)
{
        bool    isempty() { return false; }
        int     enumerate() { return 3; }
}

void test5()
{
    alias Italy!(int) Ic;
    Foo5!(int) f = new Foo5!(int);
    Ic i = cast(Ic)f;
    assert(i.isempty() == false);
    assert(i.enumerate() == 3);
}


/*******************************************/

int main (char[][] args)
{
    test1();
    test2();
    test3();
    test4();
    test5();

    printf("Success\n");
    return 0;
}
