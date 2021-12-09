// EXTRA_FILES: imports/testlambda1.d imports/testlambda2.d
module testlambdacomp;

void test1()
{
    static assert(__traits(isSame, (a, b) => a + b, (c, d) => c + d));
    static assert(__traits(isSame, a => ++a, b => ++b));
    static assert(!__traits(isSame, (int a, int b) => a + b, (a, b) => a + b));
    static assert(__traits(isSame, (a, b) => a + b + 10, (c, d) => c + d + 10));
}

class Y
{
    static int r = 5;
    int x;
    this(int x)
    {
        this.x = x;
    }
}

class A
{
    Y a;
    this(Y a)
    {
        this.a = a;
    }
}

void foo3(alias pred)()
{
    static assert(!__traits(isSame, pred, (A x, A y) => ++x.a.x + (--y.a.x)));
}

void test2()
{

    int b;
    static assert(!__traits(isSame, a => a + b, a => a + b));

    int f() { return 3;}
    static assert(__traits(isSame, a => a + f(), a => a + f()));

    class A
    {
        Y a;
        this(Y a)
        {
            this.a = a;
        }
    }

    class B
    {
        int a;
        this(int a)
        {
            this.a = a;
        }
    }

    B q = new B(7);
    alias pred = (A a, A b) => ++a.a.x + (--b.a.x);
    foo3!pred();
    static assert(!__traits(isSame, (A a) => ++a.a.x + 2, (A b) => ++b.a.x + 3));
    static assert(__traits(isSame,  pred, (A x, A y) => ++x.a.x + (--y.a.x)));
    static assert(!__traits(isSame, (B a) => ++a.a + 2, (B b) => ++b.a + 3));
    static assert(__traits(isSame, (B a) => ++a.a, (B a) => ++a.a));

    B cl = new B(7);
    static assert(!__traits(isSame, a => a + q.a, c => c + cl.a));

    class C(G)
    {
        G a;
        this(int a)
        {
            this.a = a;
        }
    }
    static assert(!__traits(isSame, (C!int a) => ++a.a, (C!int a) => ++a.a));

    struct X
    {
        int a;
    }
    static assert(__traits(isSame, (X a) => a.a + 2, (X b) => b.a + 2));

    struct T(G)
    {
        G a;
    }
    static assert(!__traits(isSame, (T!int a) => ++a.a, (T!int a) => ++a.a));

}

void test3()
{
    enum q = 10;
    static assert(__traits(isSame, (a, b) => a + b + q, (c, d) => c + d + 10));

    struct Bar
    {
        int a;
    }
    enum r1 = Bar(1);
    enum r2 = Bar(1);
    static assert(__traits(isSame, a => a + r1.a, b => b + r2.a));

    enum X { A, B, C}
    static assert(__traits(isSame, a => a + X.A, a => a + 0));
}

void foo(alias pred)()
{
    static assert(__traits(isSame, pred, (c, d) => c + d));
    static assert(__traits(isSame, (c, d) => c + d, pred));
}

void bar(alias pred)()
{
    static assert(__traits(isSame, pred, (c, d) => c < d + 7));

    enum q = 7;
    static assert(__traits(isSame, pred, (c, d) => c < d + q));

    int r = 7;
    static assert(!__traits(isSame, pred, (c, d) => c < d + r));
}
void test4()
{
    foo!((a, b) => a + b)();
    bar!((a, b) => a < b + 7);
}

int bar()
{
    return 2;
}

void testImportedFunctions(alias pred)()
{
    // imports.testalambda1.bar != imports.testlambda2.bar
    import imports.testlambda2 : bar;
    static assert(!__traits(isSame, pred, (int a) => a + bar()));
}

void testLocalGlobalFunctionScopes(alias pred)()
{
    // testlambdacomp.bar != testlambdacomp.test5.bar
    static assert(!__traits(isSame, pred, (int a) => a + bar()));

    // imports.testlambda1.bar != testlambdacomp.test5.bar
    import imports.testlambda1 : bar;
    static assert(!__traits(isSame, pred, (int a) => a + bar()));

    // functions imported from different modules are not equal
    testImportedFunctions!((int a) => a + bar())();
}

// lambda functions which contain function calls
void test5()
{

    int bar()
    {
        return 3;
    }

    // functions in the same scope
    alias pred = a => a + bar();
    alias pred2 = b => b + bar();
    static assert(__traits(isSame, pred, pred2));

    // functions in different scopes
    testLocalGlobalFunctionScopes!((int a) => a + bar())();

    int foo(int a, int b)
    {
        return 2 + a + b;
    }

    // functions with different kind of parameters
    alias preda23 = a => a + foo(2, 3);
    alias predb23 = b => b + foo(2, 3);
    alias predc24 = c => c + foo(2, 4);
    alias predd23 = (int d) => d + foo(2, 3);
    alias prede23 = (int e) => e + foo(2, 3);
    alias predf24 = (int f) => f + foo(2, 4);
    static assert(__traits(isSame, preda23, predb23));
    static assert(!__traits(isSame, predc24, predd23));
    static assert(__traits(isSame, predd23, prede23));
    static assert(!__traits(isSame, prede23, predf24));

    // functions with function calls as parameters
    static assert(!__traits(isSame, (int a, int b) => foo(foo(1, a), foo(1, b)), (int a, int b) => foo(foo(1, a), foo(2, b))));
    static assert(!__traits(isSame, (a, b) => foo(foo(1, a), foo(1, b)), (int a, int b) => foo(foo(1, a), foo(2, b))));

    float floatFunc(float a, float b)
    {
        return a + b;
    }

    static assert(__traits(isSame, a => floatFunc(a, 1.0), b => floatFunc(b, 1.0)));
    static assert(!__traits(isSame, a => floatFunc(a, 1.0), b => floatFunc(b, 2.0)));
}

void main()
{
    test1();
    test2();
    test3();
    test4();
    test5();
}
