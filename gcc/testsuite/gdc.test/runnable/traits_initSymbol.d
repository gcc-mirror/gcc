struct Zero
{
    int x;
}

void testZero()
{
    auto zeroInit = __traits(initSymbol, Zero);
    static assert(is(typeof(zeroInit) == const(void[])));

    assert(zeroInit.ptr is null);
    assert(zeroInit.length == Zero.sizeof);
}

struct NonZero
{
    long x = 1;
}

void testNonZero()
{
    auto nonZeroInit = __traits(initSymbol, NonZero);
    static assert(is(typeof(nonZeroInit) == const(void[])));

    assert(nonZeroInit.ptr);
    assert(nonZeroInit.length == NonZero.sizeof);
    assert(cast(const(long[])) nonZeroInit == [1L]);
}

class C
{
    short x = 123;
}

void testClass()
{
    auto cInit = __traits(initSymbol, C);
    static assert(is(typeof(cInit) == const(void[])));

    assert(cInit.ptr);
    assert(cInit.length == __traits(classInstanceSize, C));

    scope c = new C;
    assert((cast(void*) c)[0 .. cInit.length] == cInit);
}

struct AlignedStruct
{
    short s = 5;
    // 2 byte padding
    align(4) char c = 'c';
    // 3 byte padding
    int i = 4;
    // reduced alignment
    align(1) long l = 0xDEADBEEF;
}

void testAlignedStruct()
{
    auto init = __traits(initSymbol, AlignedStruct);

    assert(init.ptr);
    assert(init.length == AlignedStruct.sizeof);

    version (GNU)
        AlignedStruct exp = AlignedStruct();
    else
        AlignedStruct exp;
    assert(init == (cast(void*) &exp)[0 .. AlignedStruct.sizeof]);

}

class AlignedClass : C
{
    short s = 5;
    // 2 byte padding
    align(4) char c = 'c';
    // 3 byte padding
    int i = 4;
    // reduced alignment
    align(1) long l = 0xDEADBEEF;
}

void testAlignedClass()
{
    auto init = __traits(initSymbol, AlignedClass);

    assert(init.ptr);
    assert(init.length == __traits(classInstanceSize, AlignedClass));

    scope ac = new AlignedClass();
    assert(init == (cast(void*) ac)[0 .. init.length]);
}

extern (C++) class ExternCppClass
{
    int i = 4;
}

void testExternCppClass()
{
    auto init = __traits(initSymbol, ExternCppClass);

    assert(init.ptr);
    assert(init.length == __traits(classInstanceSize, ExternCppClass));

    scope ac = new ExternCppClass();
    assert(init == (cast(void*) ac)[0 .. init.length]);
}

void main()
{
    testZero();
    testNonZero();
    testClass();
    testAlignedStruct();
    testAlignedClass();
    testExternCppClass();
}
