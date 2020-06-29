// { dg-additional-sources "imports/runnable.d" }
// { dg-do run { target hw } }
// { dg-skip-if "needs gcc/config.d" { ! d_runtime } }

module runnable;

import imports.runnable;
import core.stdc.stdio;
import gcc.attribute;


/******************************************/
// https://bugzilla.gdcproject.org/show_bug.cgi?id=4

void test4()
{
    string str = "allo";
    static assert(!__traits(compiles, str.reverse));
    static assert(!__traits(compiles, str.sort));
}

/******************************************/
// https://bugzilla.gdcproject.org/show_bug.cgi?id=17

/**
 * Parameters are not copied into a frame to be accessed from
 * the method's __require function.
 */
void contractTest(string path)
{
    assert(path[0] == 't');
    assert(path.length == 9);
    assert(path[8] == 'i');
}

interface ModuleSaver
{
    void save(string str)
    in
    {
        contractTest(str);
    }
}

class ModuleWriter : ModuleSaver
{
    void save (string str)
    in {}
    body
    {
    }
}

void test17()
{
  (new ModuleWriter()).save ("test.0.mci");
}

/******************************************/
// https://bugzilla.gdcproject.org/show_bug.cgi?id=19

void test19()
{
   byte b;
   --b = b;
}

/******************************************/
// https://bugzilla.gdcproject.org/show_bug.cgi?id=24

void test24()
{
    struct S24
    {
        char[1] b;
    }

    S24 a;

    if (*a.b.ptr)
        return;
}

/******************************************/
// https://bugzilla.gdcproject.org/show_bug.cgi?id=31

class RedBlackTree(T, alias less)
{
    struct Range
    {
        @property empty() { }
    }

    Range opSlice()
    {
        return Range();
    }
}

auto redBlackTree(alias less, E)()
{
    return new RedBlackTree!(E, less);
}

void test31()
{
    redBlackTree!((a){}, double)();
}

/******************************************/
// https://bugzilla.gdcproject.org/show_bug.cgi?id=35

/**
 * Here the BinaryHeap instance uses an alias parameter and therefore
 * the instance's functions (percolateDown) need to be generated in
 * topNIndex->BinaryHeap scope and not in the declaration scope
 * (module->BinaryHeap).
 */
void topNIndex()()
{
    bool indirectLess(int a, int b)
    {
        return a > b;
    }

    auto a = BinaryHeap!(indirectLess)();
}

struct BinaryHeap(alias less)
{
    void percolateDown()
    {
        less(0, 1);
    }
}

void test35a()
{
    topNIndex();
}

/*
 * Similar as test35a but with an additional indirection.
 * The nested function chain for percolateDown should look like this:
 * topNIndex2->BinaryHeap2->percolateDown.
 */
void topNIndex2()()
{
    bool indirectLess(int a, int b)
    {
        return a > b;
    }
    auto a = BinaryHeap2!(S35b!(indirectLess)())();
}

struct S35b(alias a)
{
    void foo()
    {
        a(0, 0);
    }
}

struct BinaryHeap2(alias less)
{
    void percolateDown()
    {
        less.foo();
    }
}

void test35b()
{
    topNIndex2();
}

void test35()
{
    test35a();
    test35b();
}

/******************************************/
// https://bugzilla.gdcproject.org/show_bug.cgi?id=36

/**
 * Here getChar is a function in a template where template.isnested == false
 * but getChar still is a nested function and needs to get a static chain
 * containing test36a.
 */
void test36a()(char val)
{
    void error()
    {
    }

    void getChar()()
    {
        error();
    }

    void parseString()
    {
        getChar();
    }
}

/**
 * Similar as test36a, but a little more complicated:
 * Here getChar is nested in a struct template which is nested in a function.
 * getChar's static chain still needs to contain test36b.
 */
void test36b()(char val)
{
    void error()
    {
    }

    struct S(T)
    {
        void getChar()
        {
            error();
        }
    }


    void parseString()
    {
        S!(int)().getChar();
    }
}

/**
 * If g had accessed a, the frontend would have generated a closure.
 *
 * As we do not access it, there's no closure. We have to be careful
 * not to set a static chain for g containing test36c_1 though,
 * as g can be called from outside (here from test1c). In the end
 * we have to treat this as if everything in test36c_1 was declared
 * at module scope.
 */
auto test36c_1()
{
    int a;
    void c() {}
    class Result
    {
        int b;
        void g() { c(); /*a = 42;*/ }
    }

    return new Result();
}

void test36c()
{
    test36c_1().g();
}

/**
 * empty is a (private) function which is nested in lightPostprocess.
 * At the same time it's a template instance, so it has to be declared as
 * weak or otherwise one-only. imports/runnable.d creates another instance
 * of Regex!char to verify that.
 */
struct Parser(R)
{
    @property program()
    {
        return Regex!char();
    }
}

struct Regex(Char)
{
    @trusted lightPostprocess()
    {
        struct FixedStack(T)
        {
            @property empty() { return false; }
        }
        auto counterRange = FixedStack!uint();
    }
}

void test36d()
{
    auto parser = Parser!(char[])();
    imports.runnable.test36d_1;
}

void test36()
{
  test36a('n');
  test36b('n');
  test36c();
  test36d();
}

/******************************************/
// https://bugzilla.gdcproject.org/show_bug.cgi?id=37

struct S37
{
    int bar(const S37 s)
    {
        return 0;
    }
}

int test37()
{
    S37 s;
    return s.bar(s);
}

/******************************************/
// https://bugzilla.gdcproject.org/show_bug.cgi?id=43

void test43()
{
    import core.vararg;
    import core.stdc.stdio;

    void formatArray(ref va_list argptr)
    {
        auto a = va_arg!(const(float)[])(argptr);
        foreach(f; a)
        {
            printf("%f\n", f);
        }
    }

    void doFormat(TypeInfo[] arguments, va_list argptr)
    {
        formatArray(argptr);
    }

    void format(...)
    {
        doFormat(_arguments, _argptr);
    }

    format([1.0f, 2.0f, 3.0f]);
}

/******************************************/
// https://bugzilla.gdcproject.org/show_bug.cgi?id=47

template Foo47()
{
    void test47()
    {
        asm { "nop"; }
    }
}

mixin Foo47!();

/******************************************/
// https://bugzilla.gdcproject.org/show_bug.cgi?id=51

struct S51
{
    int x;
    int pad;

    this(this)
    {
        ++x;
    }
}

void test51()
{
    S51 s;
    auto sarr = new S51[1];
    auto sarr2 = sarr;

    // postblit all fields.
    sarr2 ~= s;

    assert (sarr2[0].x == 1);
    assert (sarr2[1].x == 1);
    assert (sarr[0].x == 0);
    assert (s.x == 0);
}

/******************************************/
// https://bugzilla.gdcproject.org/show_bug.cgi?id=57

struct S57
{
    int a;
    long b;
    // Doesn't happen for bigger structs
}

S57 bar57()
{
    return S57(4, 42);
}

void test57()
{
    S57 s = bar57();
    assert (s is S57(4, 42));
}

/******************************************/
// https://bugzilla.gdcproject.org/show_bug.cgi?id=66

void test66()
{
    int pos = 0;

    foreach(x; 0 .. 64)
    {
        ++pos %= 4;
        assert (pos != 4);
    }
}

/******************************************/
// https://bugzilla.gdcproject.org/show_bug.cgi?id=67

__vector(float[4]) d[2];  // ICE


/******************************************/
// https://bugzilla.gdcproject.org/show_bug.cgi?id=115

void test115()
{
    union U
    {
        float f;
        uint i;
    }
    float a = 123.0;
    const l = U(a);

    assert(l.i == U(a).i);
}

/******************************************/
// https://bugzilla.gdcproject.org/show_bug.cgi?id=121

immutable char C121 = void; // ICE

/******************************************/
// https://bugzilla.gdcproject.org/show_bug.cgi?id=127

int[0] test127a;     // OK
int[1][0] test127b;  // OK
int[0][1] test127c;  // ICE

/******************************************/
// https://bugzilla.gdcproject.org/show_bug.cgi?id=131

struct S131
{
    this(string ) { }
    string opAssign(string v) { return v; }
}

void test131()
{
    S131[string] s;
    s["foo"] = "bar";
}

/******************************************/
// https://bugzilla.gdcproject.org/show_bug.cgi?id=133

void delegate()[] D133;

void test133a(void delegate() dg)
{
    D133 ~= dg;
}

void test133()
{
    void nested()
    {}
    test133a(&nested);
}

/******************************************/
// https://bugzilla.gdcproject.org/show_bug.cgi?id=141

bool test141a(int a)
{
    return a > (a + 1);
}

void test141()
{
    assert(test141a(int.min) == false);
    assert(test141a(int.max) == true);
}

/******************************************/
// https://bugzilla.gdcproject.org/show_bug.cgi?id=142

@attribute("noinline")
int test142a()()
{
    return 142;
}

void test142()
{
    enum E142 = test142a();
}

/******************************************/
// https://bugzilla.gdcproject.org/show_bug.cgi?id=171

void test171a()
{
    int count = 0;
    short a = -1;
    while (a != 0)
    {
        a >>>= 1;
        count++;
        assert(count <= 16);
    }
}

void test171b()
{
    uint[3] lhs = [99, 201, 300],
            rhs = [-1, 0, 0];
    long t = 0;

    for (int i = 0; i < 3; i++)
    {
        t += lhs[i];
        t -= rhs[i];
        lhs[i] = cast(uint) t;
        t >>= uint.sizeof * 8;
    }

    assert(lhs == [100, 200, 300]);
}

void test171()
{
    test171a();
    test171b();
}

/******************************************/
// https://bugzilla.gdcproject.org/show_bug.cgi?id=179

struct S179a
{
    @disable this(this);
}

struct S179b
{
    S179a s1;
    void connect() { printf("this=%p\n", &this); }
}

class C179
{
    private S179b s2;
    ref S179b value() @property
    {
        printf("this=%p\n", &s2);
        return s2;
    }
}

void test179()
{
    C179 a = new C179;
    a.value.connect();
}

/******************************************/
// https://bugzilla.gdcproject.org/show_bug.cgi?id=183

struct S183a
{
    union I183a
    {
        struct
        {
            double x, y, z;
        }
        struct
        {
            double a, b, c;
        }
    }

    I183a inner;

    this(double x, double y, double z)
    {
        this.inner.x = x;
        this.inner.y = y;
        this.inner.z = z;
    }
}

struct S183b
{
    @property get()
    {
        union Buf
        {
            void[0] result;
        }
        const Buf buf = { };
        return buf.result;
    }
}

struct S183c
{
    @property get()
    {
        union Buf
        {
            TypeInfo info;
            void[0] result;
        }
        const Buf buf = { };
        return buf.result;
    }
}

void test183()
{
    auto v1 = S183a(0, 0, 0);
    auto v2 = S183b().get;
    auto v3 = S183c().get;
}

/******************************************/
// https://bugzilla.gdcproject.org/show_bug.cgi?id=186

struct S186
{
    union
    {
        struct
        {
            ubyte fieldA;
            byte  fieldB = -1;
            byte fieldC = -1;
        }
        size_t _complete;
    }

    this(size_t complete)
    {
        this._complete = complete;
    }
}

static if (size_t.sizeof == 8)
    enum checkval = 0x0200000000000002;
else
    enum checkval = 0x02000002;

void check186(in S186 obj, byte fieldB)
{
    assert(obj.fieldA == 2);
    assert(obj.fieldB == 0);
    assert(obj.fieldC == 0);
    assert(obj._complete == checkval);
    assert(fieldB == 0);
}

void test186a(size_t val)
{
    S186 obj = S186(val);
    check186(obj, obj.fieldB);

    assert(obj.fieldA == 2);
    assert(obj.fieldB == 0);
    assert(obj.fieldC == 0);
    assert(obj._complete == checkval);

    obj = S186(val);
    check186(obj, obj.fieldB);

    assert(obj.fieldA == 2);
    assert(obj.fieldB == 0);
    assert(obj.fieldC == 0);
    assert(obj._complete == checkval);
}

void test186()
{
    test186a(checkval);
}

/******************************************/
// https://bugzilla.gdcproject.org/show_bug.cgi?id=187

align(1) struct S187b
{
    align(1)
    {
        uint unpaddedA;
        ushort unpaddedB;
    }
}

struct S187a
{
    S187b[3] unpaddedArray;
    ubyte wontInitialize = ubyte.init;
}

struct S187
{
    S187a interesting;
}


void prepareStack()
{
    byte[255] stackGarbage;
    foreach(i, ref b; stackGarbage)
    {
        b  = cast(byte)(-i);
    }
}

void test187()
{
    prepareStack();
    auto a = S187(S187a());
    assert(a.interesting.wontInitialize == 0);
}

/******************************************/
// https://bugzilla.gdcproject.org/show_bug.cgi?id=191

class C191
{
    int count = 0;

    void testA()
    {
        class Inner
        {
            void test()
            {
                void localFunction()
                {
                    if (++count != 5)
                        testA();
                }
                localFunction();
            }
        }
        scope ic = new Inner();
        ic.test();
    }

    void testB()
    {
        class Inner
        {
            void test()
            {
                void localFunction()
                {
                    void anotherLocalFunction()
                    {
                        if (++count != 10)
                            testB();
                    }
                    anotherLocalFunction();
                }
                localFunction();
            }
        }
        scope ic = new Inner();
        ic.test();
    }

    void testC()
    {
        class Inner
        {
            int a = 1;

            void test()
            {
                void localFunction()
                {
                    count += a;
                    if (count != 15)
                        testC();
                    assert(a == 1);
                }
                localFunction();
            }
        }
        scope ic = new Inner();
        ic.test();
    }

    void testD()
    {
        class Inner
        {
            void test()
            {
                int a = 1;

                void localFunction()
                {
                    count += a;
                    if (count != 20)
                        testD();
                    assert(a == 1);
                }
                localFunction();
            }
        }
        scope ic = new Inner();
        ic.test();
    }

    void testE()
    {
        class Inner
        {
            int a = 1;

            void test()
            {
                void localFunction()
                {
                    void anotherLocalFunction()
                    {
                        count += a;
                        if (count != 25)
                            testE();
                        assert(a == 1);
                    }

                    anotherLocalFunction();
                }

                localFunction();
            }
        }
        scope ic = new Inner();
        ic.test();
    }

    void testF()
    {
        class Inner
        {
            void test()
            {
                int a = 1;

                void localFunction()
                {
                    void anotherLocalFunction()
                    {
                        count += a;
                        if (count != 30)
                            testF();
                        assert(a == 1);
                    }

                    anotherLocalFunction();
                }

                localFunction();
            }
        }
        scope ic = new Inner();
        ic.test();
    }

    void testG()
    {
        class Inner
        {
            void test()
            {
                void localFunction()
                {
                    int a = 1;

                    void anotherLocalFunction()
                    {
                        count += a;
                        if (count != 35)
                            testG();
                        assert(a == 1);
                    }

                    anotherLocalFunction();
                }

                localFunction();
            }
        }
        scope ic = new Inner();
        ic.test();
    }
}

void test191()
{
    scope oc = new C191();
    oc.testA();
    assert(oc.count == 5);

    oc.testB();
    assert(oc.count == 10);

    oc.testC();
    assert(oc.count == 15);

    oc.testD();
    assert(oc.count == 20);

    oc.testE();
    assert(oc.count == 25);

    oc.testF();
    assert(oc.count == 30);

    oc.testG();
    assert(oc.count == 35);
}

/******************************************/
// https://bugzilla.gdcproject.org/show_bug.cgi?id=194

auto test194(ref bool overflow)
{
    import core.checkedint;

    return adds(1, 1, overflow);
}

/******************************************/
// https://bugzilla.gdcproject.org/show_bug.cgi?id=196

class C196
{
    int a;
}

struct S196
{
    int a;
}

void test196()
{
    __gshared c = new C196();
    __gshared s = new S196(0);
    c.a = 1;
    s.a = 1;
}

/******************************************/
// https://bugzilla.gdcproject.org/show_bug.cgi?id=198

struct S198a
{
    union
    {
        float[3] v;
        struct
        {
            float x;
            float y;
            float z;
        }
    }

    this(float x_, float y_, float z_)
    {
        x = x_;
        y = y_;
        z = z_;
    }

    ref S198a opOpAssign(string op)(S198a operand)
    if (op == "+")
    {
        x += operand.x;
        y += operand.y;
        z += operand.z;
        return this;
    }
}

struct S198b
{
    @property get()
    {
        union Buf
        {
            void[0] result;
        }
        const Buf buf = { };
        return buf.result;
    }
}

struct S198c
{
    @property get()
    {
        union Buf
        {
            TypeInfo info;
            void[0] result;
        }
        const Buf buf = { };
        return buf.result;
    }
}


auto test198()
{
    S198a sum = S198a(0, 0, 0);

    foreach(size_t v; 0 .. 3)
        sum += S198a(1, 2, 3);

    assert(sum.v == [3, 6, 9]);
}

/******************************************/
// https://bugzilla.gdcproject.org/show_bug.cgi?id=200

void test200a(double x, double y)
{
  const double y2 = x + 1.0;
  assert(y == y2);
}

void test200()
{
  const double x = .012;
  const double y = x + 1.0;
  test200a(x, y);
}

/******************************************/
// https://bugzilla.gdcproject.org/show_bug.cgi?id=210

struct S210
{
    ubyte a;
    uint b;
}

union U210
{
    S210 a;
    uint b;
}

S210 test210a()
{
    S210 s = S210(1, 2);
    return s;
}

S210[2] test210b()
{
    S210[2] s = [S210(1, 2), S210(3, 4)];
    return s;
}

U210 test210c()
{
    U210 s = U210(S210(1, 2));
    return s;
}

U210[2] test210d()
{
    U210[2] s = [U210(S210(1, 2)), U210(S210(3, 4))];
    return s;
}

void test210()
{
    S210 a = S210(1, 2);
    assert(a == S210(1, 2));
    assert(a == test210a());
    assert(a != S210(2, 1));

    S210[2] b = [S210(1, 2), S210(3, 4)];
    assert(b == [S210(1, 2), S210(3, 4)]);
    assert(b == test210b());
    assert(b != [S210(2, 1), S210(3, 4)]);

    U210 c = U210(S210(1, 2));
    assert(c == U210(S210(1, 2)));
    assert(c == test210c());
    assert(c != U210(S210(2, 1)));

    U210[2] d = [U210(S210(1, 2)), U210(S210(3, 4))];
    assert(d == [U210(S210(1, 2)), U210(S210(3, 4))]);
    assert(d == test210d());
    assert(d != [U210(S210(2, 1)), U210(S210(3, 4))]);
}

/******************************************/
// https://bugzilla.gdcproject.org/show_bug.cgi?id=240

void test240a(int a, int b)
{
    assert(a == 0);
    assert(b == 0);
}

void test240()
{
    int a = 0;
    test240a(a, a++);
    assert(a == 1);
}

/******************************************/
// https://bugzilla.gdcproject.org/show_bug.cgi?id=242

struct S242
{
    enum M = S242();
    int a = 42;

    auto iter()
    {
        this.a = 24;
        return this;
    }
}

S242 test242a()
{
    return S242.M.iter;
}

void test242()
{
    assert(test242a() == S242(24));
}

/******************************************/
// https://bugzilla.gdcproject.org/show_bug.cgi?id=248

class C248b
{
    bool isintegral()
    {
        return false;
    }
}

class C248a
{
    int count = 0;

    C248b getMemtype()
    {
        count++;
        return new C248b();
    }
}

class C248
{
    C248a sym;

    this()
    {
        this.sym = new C248a();
    }

    bool isintegral()
    {
        return sym.getMemtype().isintegral();
    }
}

void test248()
{
    C248 e = new C248();
    e.isintegral();
    assert(e.sym.count == 1);
}

/******************************************/
// https://bugzilla.gdcproject.org/show_bug.cgi?id=250

void test250()
{
    struct S
    {
        string data;
    }

    auto a = S("hello");
    auto b = S("hello".dup);

    assert(a.data == b.data);
    assert(a == b);
    assert([a] == [b]);
}

/******************************************/
// https://bugzilla.gdcproject.org/show_bug.cgi?id=253

interface A253
{
    void test253(int[int]);
}

interface C253 : A253
{
}

class D253 : B253, C253
{
}

/******************************************/
// https://bugzilla.gdcproject.org/show_bug.cgi?id=273

class B273
{
    B273[] members;
}

class D273 : B273
{
}

void test273()
{
    auto noPointers = ClassInfo.ClassFlags.noPointers;
    assert((B273.classinfo.m_flags & noPointers) == 0);
    assert((D273.classinfo.m_flags & noPointers) == 0);
}

/******************************************/
// https://bugzilla.gdcproject.org/show_bug.cgi?id=285

inout(char)[] test285a(inout(char)* s) @nogc @system pure nothrow
{
    import core.stdc.string : strlen;
    return s ? s[0 .. strlen(s)] : null;
}

void test285()
{
    assert(test285a(null) == null);
    assert(test285a("foo") == "foo");
}

/******************************************/
// https://bugzilla.gdcproject.org/show_bug.cgi?id=286

void test286()
{
    struct K286
    {
        int count;
        this(this)
        {
            count++;
        }
    }

    struct S286
    {
        int data;
        this(K286 key)
        {
            data = key.count;
        }
    }

    S286 getData(K286 key)
    {
        static S286[K286] getCache;
        auto p = key in getCache;
        if (p)
            return *p;
        return (getCache[key] = S286(key));
    }

    auto s = getData(K286());
    if (s.data == 0)
        assert(0);
}

/******************************************/
// https://bugzilla.gdcproject.org/show_bug.cgi?id=309

void test309()
{
    creal f1 = +0.0 + 0.0i;
    creal f2 = +0.0 - 0.0i;
    creal f3 = -0.0 + 0.0i;
    creal f4 = +0.0 + 0.0i;

    assert(f1 !is f2);
    assert(f1 !is f3);
    assert(f2 !is f3);
    assert(f1 is f4);

    assert(!(f1 is f2));
    assert(!(f1 is f3));
    assert(!(f2 is f3));
    assert(!(f1 !is f4));

    struct CReal
    {
        creal value;
    }

    CReal s1 = CReal(+0.0 + 0.0i);
    CReal s2 = CReal(+0.0 - 0.0i);
    CReal s3 = CReal(-0.0 + 0.0i);
    CReal s4 = CReal(+0.0 + 0.0i);

    assert(s1 !is s2);
    assert(s1 !is s3);
    assert(s2 !is s3);
    assert(s1 is s4);

    assert(!(s1 is s2));
    assert(!(s1 is s3));
    assert(!(s2 is s3));
    assert(!(s1 !is s4));
}

/******************************************/

void main()
{
    test4();
    test17();
    test35();
    test36();
    test43();
    test51();
    test57();
    test66();
    test115();
    test131();
    test133();
    test141();
    test179();
    test186();
    test187();
    test191();
    test196();
    test198();
    test200();
    test210();
    test240();
    test242();
    test248();
    test250();
    test273();
    test285();
    test286();
    test309();

    printf("Success!\n");
}
