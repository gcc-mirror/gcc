// EXTRA_CPP_SOURCES: externmangle.cpp

extern(C++):

struct Foo(X)
{
    X* v;
}


struct Boo(X)
{
    X* v;
}


void test1(Foo!int arg1);
void test2(int* arg2, Boo!(int*) arg1);


struct Test3(int X, int Y)
{
}

void test3(Test3!(3,3) arg1);

void test4(Foo!(int*) arg1, Boo!(int*) arg2, Boo!(int*) arg3, int*, Foo!(double));

void test5(Foo!(int*) arg1, Boo!(int*) arg2, Boo!(int*) arg3);


struct Goo
{
    struct Foo(X)
    {
        X* v;
    }

    struct Boo(X)
    {
        struct Xoo(Y)
        {
            Y* v;
        };
        X* v;
    }


    void test6(Foo!(Boo!(Foo!(void))) arg1);
    void test7(Boo!(void).Xoo!(int) arg1);
}

struct P1
{
    struct Mem(T)
    {
    }
}

struct P2
{
    struct Mem(T)
    {
    }
}

void test8(P1.Mem!int, P2.Mem!int);
void test9(Foo!(int**), Foo!(int*), int**, int*);


interface Test10
{
    private final void test10();
    public final void test11();
    protected final void test12();
    public final void test13() const;

    private void test14();
    public void test15();
    protected void test16();

    private static void test17();
    public static void test18();
    protected static void test19();
};

Test10 Test10Ctor();
void Test10Dtor(ref Test10 ptr);

struct Test20
{
    __gshared:
    private extern int test20;
    protected extern int test21;
    public extern int test22;
};


int test23(Test10*, Test10, Test10**, const(Test10));
int test23b(const Test10*, const Test10, Test10);

void test24(int function(int,int));

void test25(int[291][6][5]* arr);
int test26(int[291][6]* arr);

void test27(int, ...);
void test28(int);

void test29(float);
void test30(const float);

struct Array(T)
{
    int dim;
}


interface Module
{
    public static void imports(Module);
    public static int dim(Array!Module*);
};

ulong testlongmangle(int a, uint b, long c, ulong d);

__gshared extern int[2][2][2] test31;
__gshared extern int* test32;


alias int function(Expression , void* ) apply_fp_t;

interface Expression
{
    public final int apply(apply_fp_t fp, apply_fp_t fp2, void* param);
    public final int getType();
    public static Expression create(int);
    public static void dispose(ref Expression);
}

//int test34(int[0][0]*);
version(CRuntime_Microsoft){}
else
{
    int test35(real arg);
}

const(char)* test36(const(char)*);

final class Test37
{
    static Test37 create()
    {
        return new Test37;
    }

    bool test()
    {
        return true;
    }
}

bool test37();

interface Test38
{
     final int test(int, ...);
     public static Test38 create();
     public static void dispose(ref Test38);
}

extern(C++) int test39cpp(C2!char, S2!(int)*);

extern(C++, class)
struct S1
{
    private int val;
    static S1* init(int);
    int value();
}

extern(C++, class)
struct S2(T)
{
    private T val;
    static S2!T* init(int);
    T value();
}

extern(C++, struct)
class C1
{
    const(char)* data;

    static C1 init(const(char)* p);
    const(char)* getDataCPP();
    extern(C++) const(char)* getDataD()
    {
        return data;
    }
}

extern(C++, struct)
class C2(T)
{
    const(T)* data;

    static C2!T init(const(T)* p);
    const(T)* getData();
}

void test39()
{
    S1* s1 = S1.init(42);
    assert(s1.value == 42);
    assert(S2!int.init(43).value == 43);
    const(char)* ptr = "test".ptr;
    C1 c1 = C1.init(ptr);
    assert(c1.getDataCPP() == ptr);
    assert(c1.getDataD() == ptr);
    C2!char c2 = C2!char.init(ptr);
    assert(c2.getData() == ptr);
    auto result = test39cpp(c2, S2!int.init(43));
    assert(result == 0);
}


void main()
{
    test1(Foo!int());
    test2(null, Boo!(int*)());
    test3(Test3!(3,3)());
    test4(Foo!(int*)(), Boo!(int*)(), Boo!(int*)(), null, Foo!(double)());
    test5(Foo!(int*)(), Boo!(int*)(), Boo!(int*)());
    Goo goo;
    goo.test6(Goo.Foo!(Goo.Boo!(Goo.Foo!(void)))());
    goo.test7(Goo.Boo!(void).Xoo!(int)());

    test8(P1.Mem!int(), P2.Mem!int());
    test9(Foo!(int**)(), Foo!(int*)(), null, null);

    auto t10 = Test10Ctor();
    scope(exit) Test10Dtor(t10);

    t10.test10();
    t10.test11();
    t10.test12();
    t10.test13();
    t10.test14();
    t10.test15();
    t10.test16();
    t10.test17();
    t10.test18();
    t10.test19();

    assert(Test20.test20 == 20);
    assert(Test20.test21 == 21);
    assert(Test20.test22 == 22);

    assert(test23(null, null, null, null) == 1);
    assert(test23b(null, null, null) == 1);

    extern(C++) static int cb(int a, int b){return a+b;}

    test24(&cb);
    int[291][6][5] arr;
    arr[1][1][1] = 42;
    test25(&arr);
    assert(test26(&arr[0]) == 42);

    test27(3,4,5);
    test28(3);

    test29(3.14f);
    test30(3.14f);

    auto t32 = &Module.imports;
    Array!Module arr2;
    arr2.dim = 20;
    assert(Module.dim(&arr2) == 20);

    assert(testlongmangle(1, 2, 3, 4) == 10);
    assert(test31 == [[[1, 1], [1, 1]], [[1, 1], [1, 1]]]);
    assert(test32 == null);

    auto ee = Expression.create(42);
    extern(C++) static int efun(Expression e, void* p)
    {
        return cast(int)(cast(size_t)p ^ e.getType());
    }

    extern(C++) static int efun2(Expression e, void* p)
    {
        return cast(int)(cast(size_t)p * e.getType());
    }

    auto test33 = ee.apply(&efun, &efun2, cast(void*)&Expression.create);
    assert(test33 == cast(int)(cast(size_t)cast(void*)&Expression.create ^ 42) * cast(int)(cast(size_t)cast(void*)&Expression.create * 42));
    Expression.dispose(ee);
    assert(ee is null);
    //assert(test34(null) == 0);
    version(CRuntime_Microsoft){}
    else
    {
        assert(test35(3.14L) == 3);
    }
    const char* hello = "hello";
    assert(test36(hello) == hello);
    assert(test37());
    auto t38 = Test38.create();
    assert(t38.test(1, 2, 3) == 1);
    Test38.dispose(t38);
    test39();
}
