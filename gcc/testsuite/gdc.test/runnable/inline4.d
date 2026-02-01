struct S
{
    int a;
    int b;
    int c;

    bool test(int a_, int b_, int c_) inout
    {
        return a_ == a && b_ == b && c_ == c;
    }
}

__gshared S globalS = S(2, 3, 4);
__gshared S globalS2 = S();
immutable S immutableS = S(9, 8, 7);

S getVal()
{
    // Returns by value
    return globalS;
}

ref S getRef()
{
    // Returns by reference
    return globalS;
}

ref S getRefRvalue() __rvalue
{
    // Takes ownership of globalS
    return globalS;
}

S getImmutable()
{
    return immutableS;
}

__gshared int globalDtorCount;

struct D
{
    int d;

    this(D rhs)
    {
        d = rhs.d;
        rhs.d = 0xdeadbeef;
    }

    this(ref D rhs)
    {
        d = 12345;
    }

    ~this()
    {
        if (d == 123456)
            globalDtorCount++;
        d = 0xdeadbeef;
    }
}

__gshared D globalD = D(123456);
__gshared D globalD2 = D(10000);

void resetD()
{
    globalDtorCount = 0;
    globalD.d = 123456;
    globalD2.d = 10000;
}

D getD()
{
    // Returns by value
    return globalD;
}

D getRvalueD()
{
    // Returns by moving globalD, but should not have reference semantics
    // Tests if the inliner incorrectly applies reference semantics
    return __rvalue(globalD);
}

ref D getRefD()
{
    // Returns by reference
    return globalD;
}

ref D getRefRvalueD() __rvalue
{
    // Takes ownership of globalS
    return globalD;
}

/************************************/
// Test inlining of return by value

S funcVal1()
{
    return getVal();
}

S funcVal2()
{
    return getRef();
}

S funcVal3()
{
    return getRefRvalue();
}

S funcVal4()
{
    return globalS.a ? immutableS : globalS;
}

S funcVal5()
{
    return globalS.a ? globalS : S();
}

D funcVal6()
{
    return getD();
}

D funcVal7()
{
    return getRvalueD();
}

D funcVal8()
{
    return getRefD();
}

D funcVal9()
{
    return getRefRvalueD();
}

D funcVal10()
{
    return __rvalue(globalS.a ? globalD : D());
}

void consumeD(D d) {}

void testValueReturn()
{
    // Returning by value should not change the source object
    getVal().a++;
    assert(globalS.test(2, 3, 4));
    getImmutable().a++;
    assert(immutableS.test(9, 8, 7));
    funcVal1().a++;
    assert(globalS.test(2, 3, 4));
    funcVal2().c++;
    assert(globalS.test(2, 3, 4));
    funcVal3().c++;
    assert(globalS.test(2, 3, 4));
    funcVal4().a++;
    assert(immutableS.test(9, 8, 7));
    funcVal5().a++;
    assert(globalS.test(2, 3, 4));

    consumeD(getD());
    assert(globalD.d == 123456);
    assert(globalDtorCount == 0);

    consumeD(__rvalue(getD()));
    assert(globalD.d == 123456);
    assert(globalDtorCount == 0);

    consumeD(getRvalueD());
    assert(globalD.d == 0xdeadbeef);
    assert(globalDtorCount == 1);
    resetD();

    consumeD(__rvalue(getRvalueD()));
    assert(globalD.d == 0xdeadbeef);
    assert(globalDtorCount == 1);
    resetD();

    consumeD(funcVal6());
    assert(globalD.d == 123456);
    assert(globalDtorCount == 0);

    consumeD(__rvalue(funcVal6()));
    assert(globalD.d == 123456);
    assert(globalDtorCount == 0);

    consumeD(funcVal7());
    assert(globalD.d == 0xdeadbeef);
    assert(globalDtorCount == 1);
    resetD();

    consumeD(__rvalue(funcVal7()));
    assert(globalD.d == 0xdeadbeef);
    assert(globalDtorCount == 1);
    resetD();

    consumeD(funcVal8());
    assert(globalD.d == 123456);
    assert(globalDtorCount == 0);

    consumeD(__rvalue(funcVal8()));
    assert(globalD.d == 123456);
    assert(globalDtorCount == 0);

    consumeD(funcVal9());
    assert(globalD.d == 0xdeadbeef);
    assert(globalDtorCount == 1);
    resetD();

    consumeD(__rvalue(funcVal9()));
    assert(globalD.d == 0xdeadbeef);
    assert(globalDtorCount == 1);
    resetD();

    consumeD(funcVal10());
    assert(globalD.d == 123456);
    assert(globalDtorCount == 0);

    consumeD(__rvalue(funcVal10()));
    assert(globalD.d == 123456);
    assert(globalDtorCount == 0);
}


/************************************/
// Test inlining of return by reference

ref S funcRef1()
{
    return getRef();
}

ref S funcRef2()
{

    return globalS.a ? globalS : globalS2;
}

ref D funcRef3()
{
    return getRefD();
}

ref D funcRef4()
{
    return __rvalue(globalS.a ? globalD : globalD2);
}

void consumeRefD(ref D d)
{
    assert(d.d == 123456);
    consumeD(__rvalue(d));
    assert(d.d == 0xdeadbeef);
}

void testRefReturn()
{
    // Returning by ref should mutate the source object
    funcRef1().a++;
    assert(globalS.test(3, 3, 4));
    funcRef2().c++;
    assert(globalS.test(3, 3, 5));

    // Passing ref to value parameter should trigger a copy
    resetD();
    consumeD(funcRef3());
    assert(globalD.d == 123456);
    assert(globalDtorCount == 0);

    // ... but not if there is __rvalue
    consumeD(__rvalue(funcRef3()));
    assert(globalD.d == 0xdeadbeef);
    assert(globalDtorCount == 1);

    // ditto
    resetD();
    consumeD(funcRef4());
    assert(globalD.d == 123456);
    assert(globalDtorCount == 0);
    consumeD(__rvalue(funcRef4()));
    assert(globalD.d == 0xdeadbeef);
    assert(globalDtorCount == 1);

    resetD();
    consumeRefD(funcRef3());

    resetD();
    consumeRefD(funcRef4());
}

/************************************/
// Test inlining of return by rvalue reference

ref S funcRvalueRef1() __rvalue
{
    return getRef();
}

ref S funcRvalueRef2() __rvalue
{
    return globalS.a ? globalS : globalS2;
}

ref D funcRvalueRef3() __rvalue
{
    return getRefD();
}

ref D funcRvalueRef4() __rvalue
{
    return globalS.a ? globalD : globalD2;
}

void consumeRvalueRefD(D d)
{
    assert(d.d == 123456);
    consumeD(__rvalue(d));
    assert(d.d == 0xdeadbeef);
}

void testRvalueRefReturn()
{
    // rvalue ref is ref
    funcRvalueRef1().a++;
    assert(globalS.test(4, 3, 5));
    funcRvalueRef2().c++;
    assert(globalS.test(4, 3, 6));

    resetD();
    consumeRvalueRefD(funcRvalueRef3());

    resetD();
    consumeRvalueRefD(funcRvalueRef4());
}

/************************************/
// https://github.com/dlang/dmd/issues/22157
class Test22157
{
    override string toString()
    {
        auto e = { return cast() super; } ();
        return null;
    }
}

/************************************/
// https://issues.dlang.org/show_bug.cgi?id=22089

struct S22089
{
    @disable this(ref return scope typeof(this) rhs);
    @disable this(this);
    void* cons;
    this(int i)
    {
        cons = cast(void*)&this;
    }
    void start()
    {
        void* s = cast(void*)&this;
        assert(cons == s);
    }
}

auto fun22089()
{
    return S22089(42);
}

void test22089()
{
    auto op = fun22089();
    op.start();
}

/************************************/

void main()
{
    testValueReturn();
    testRefReturn();
    testRvalueRefReturn();
    test22089();
}
