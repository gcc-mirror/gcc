// https://issues.dlang.org/show_bug.cgi?id=23103
// Issue 23103 - static initialization of associative arrays is not implemented

nothrow @safe:

/////////////////////////////////////////////

int[int] globalAA = [1: 10, 2: 20];

void testSimple()
{
    assert(globalAA[1] == 10);
    assert(globalAA[2] == 20);
    assert(!(30 in globalAA));

    foreach (i; 0 .. 1000)
    {
        globalAA[i] = i * 10;
        assert(globalAA[i] == i * 10);
    }
}

/////////////////////////////////////////////

struct Composit
{
    string[string][] aa;
}

auto getAA() { return ["a": "A"]; }

immutable Composit compositAA = Composit([getAA(), ["b": "B"]]);

void testComposit() pure
{
    assert(compositAA.aa[0]["a"] == "A");
    assert(compositAA.aa[1]["b"] == "B");
}

/////////////////////////////////////////////

struct Destructing
{
    int v;
    static int destructorsCalled = 0;

    ~this() nothrow
    {
        // FIXME: the lowering to newaa calls the destructor at CTFE, so we can't modify globals in it
        if (!__ctfe)
            destructorsCalled++;
    }
}

struct Key
{
    int v;
    bool opEquals(ref const Key o) const { return v == o.v; }
    size_t toHash() const nothrow { return v; }
}

Destructing[Key] dAa = [Key(1): Destructing(10), Key(2): Destructing(20)];

void testDestructor()
{
    assert(dAa[Key(1)].v == 10);
    assert(dAa[Key(2)].v == 20);
    assert(Destructing.destructorsCalled == 0);
    dAa[Key(1)] = Destructing(100);
    assert(dAa[Key(1)].v == 100);
    assert(Destructing.destructorsCalled == 1);
}

/////////////////////////////////////////////

enum A
{
    x, y, z
}

struct S
{
    string[A] t = [A.x : "A.x", A.y : "A.y"];
}

void testStructInit()
{
    S s;
    assert(s.t[A.x] == "A.x");
    assert(s.t[A.y] == "A.y");
}

struct S2
{
    string[A] t = [A.x : "A.x", A.y : "A.y"];
}

bool testStructInitCTFE()
{
    S2 s2;
    assert(s2.t[A.x] == "A.x");
    assert(s2.t[A.y] == "A.y");
    return true;
}
static assert(testStructInitCTFE());

/////////////////////////////////////////////

class C
{
    string[int] t = [0 : "zero"];
}

void testClassInit()
{
    C c = new C();
    assert(c.t[0] == "zero");
}

class C2
{
    string[int] t = [0 : "zero"];
}

bool testClassInitCTFE()
{
    C2 c2 = new C2();
    assert(c2.t[0] == "zero");
    return true;
}
static assert(testClassInitCTFE());

/////////////////////////////////////////////

immutable(string)[immutable(int)] immutableAA = [1: "one", 2: "two"];

void testImmutable()
{
    assert(immutableAA[1] == "one");
    assert(immutableAA[2] == "two");
}

/////////////////////////////////////////////

// https://issues.dlang.org/show_bug.cgi?id=24209
void testLocalStatic() @trusted
{
    static int[int] aa0 = [1: 2];
    __gshared string[string] aa1 = null;

    assert(aa0[1] == 2);
    assert(aa1.length == 0);
}

/////////////////////////////////////////////

void main()
{
    testSimple();
    testComposit();
    testDestructor();
    testStructInit();
    testClassInit();
    testImmutable();
    testLocalStatic();
}
