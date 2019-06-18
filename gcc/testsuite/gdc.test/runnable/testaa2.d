/*
RUNNABLE_PHOBOS_TEST
PERMUTE_ARGS:
RUN_OUTPUT:
---
foo()
foo() 2
foo() 3
foo() 4
Success
---
*/

extern(C) int printf(const char*, ...);

/************************************************/

int[string] a;

size_t foo(immutable char [3] s)
{
    printf("foo()\n");
    int[string] b;
    string[] key;
    int[] value;
    printf("foo() 2\n");
    key = a.keys;
    printf("foo() 3\n");
    value = a.values;
    printf("foo() 4\n");
    return a.length + b.length;
}

void foo2()
{
    int[string] c;
    string[] key;
    int[] value;
    int i;

    assert(c.length == 0);
    key = c.keys;
    assert(key.length == 0);
    value = c.values;
    assert(value.length == 0);

    c["foo"] = 3;
    assert(c["foo"] == 3);
    assert(c.length == 1);
    key = c.keys;
    assert(key.length == 1);
    value = c.values;
    assert(value.length == 1);
    assert(value[0] == 3);

    c["bar"] = 4;
    assert(c["bar"] == 4);
    assert(c.length == 2);
    key = c.keys;
    assert(key.length == 2);
    value = c.values;
    assert(value.length == 2);

    const fooIndex = key[1] == "foo";
    assert(key[fooIndex] == "foo" && value[fooIndex] == 3);
    assert(key[1 - fooIndex] == "bar" && value[1 - fooIndex] == 4);

    assert("foo" in c);
    c.remove("foo");
    assert(!("foo" in c));
    assert(c.length == 1);

    assert("bar" in c);
    c.remove("bar");
    assert(!("bar" in c));
    assert(c.length == 0);
}

void testaa()
{
    size_t i = foo("abc");
    assert(i == 0);

    foo2();
}

/************************************************/

void test1899()
{
    int[3][string] AA;
    int[3] x = [5,4,3];
    AA["abc"] = x;
    assert(AA["abc"] == x);
    AA["def"] = [1,2,3];
    assert(AA["def"]==[1,2,3]);
}

/************************************************/

void foo4523()
{
   int[string] aa = ["test":0, "test2":1];

   bool found = aa.remove("test");
   assert(found);
   bool notfound = aa.remove("nothing");
   assert(!notfound);
}

void test4523()
{
    foo4523();
    static assert({ foo4523(); return true; }());
}

/************************************************/
// https://issues.dlang.org/show_bug.cgi?id=3825

import std.math;    // necessary for ^^=
void test3825()
{
    // Check for RangeError is thrown
    bool thrown(T)(lazy T cond)
    {
        import core.exception;
        bool f = false;
        try {
            cond();
        } catch (RangeError e) { f = true; }
        return f;
    }

    int[int] aax;
    int[][int] aay;

    aax = null, aay = null;
    assert(thrown(aax[0]));
    assert(thrown(aax[0]   = aax[0]));  // rhs throws
    assert(thrown(aax[0]  += aax[0]));  // rhs throws
    assert(thrown(aax[0] ^^= aax[0]));  // rhs throws
    assert(thrown(aay[0]  ~= aay[0]));  // rhs throws
    aax = null;   aax[0]   = 1;  assert(aax[0] ==  1);  // setting aax[0] is OK
    aax = null;   aax[0]  += 1;  assert(aax[0] == +1);  // setting aax[0] to 0 and modify it is OK
    aax = null;   aax[0] ^^= 1;  assert(aax[0] ==  0);  // setting aax[0] to 0 and modify it is OK
    aay = null;   aay[0]  ~= []; assert(aay[0] == []);  // setting aay[0] to 0 and modify it is OK
    aax = null; ++aax[0];        assert(aax[0] == +1);  // setting aax[0] to 0 and modify it is OK
    aax = null; --aax[0];        assert(aax[0] == -1);  // setting aax[0] to 0 and modify it is OK

    aax = [0:0], aay = [0:null];
    assert(thrown(aax[aax[1]]   = 1));  // accessing aax[1] in key part throws
    assert(thrown(aax[aax[1]]  += 1));  // accessing aax[1] in key part throws
    assert(thrown(aax[aax[1]] ^^= 1));  // accessing aax[1] in key part throws
    assert(thrown(aay[aax[1]]  ~= [])); // accessing aax[1] in key part throws

    //assert(thrown(aax[(  aax[1], 0)] = 0));
    /* accessing aax[1] in key part, why doesn't throw?
     * Because, in aax[(aax[1], 0)], aax[1] is in lhs of comma expression, and is treated
     * it has no side effect. Then optimizer eliminate it completely, and
     * whole expression succeed to run in runtime. */
    int n = 0;
    assert(thrown(aax[((){ n=aax[1]; return 0;}())] = 0)); // accessing aax[1] in key part, throws OK

    // This works as expected.
    int[int][int] aaa;
    aaa[0][0] = 0;              assert(aaa[0][0] == 0); // setting aaa[0][0] is OK

    // real test cases
    void bug3825()
    {
        string[] words = ["how", "are", "you", "are"];

        int[string] aa1;
        foreach (w; words)
            aa1[w] = ((w in aa1) ? (aa1[w] + 1) : 2);
        //writeln(aa1); // Prints: [how:1,you:1,are:2]

        int[string] aa2;
        foreach (w; words)
            if (w in aa2)
                aa2[w]++;
            else
                aa2[w] = 2;
        //writeln(aa2); // Prints: [how:2,you:2,are:3]

        assert(aa1 == aa2);
        assert(aa1 == ["how":2, "you":2, "are":3]);
        assert(aa2 == ["how":2, "you":2, "are":3]);
    }
    void bug5021()
    {
        int func()
        {
            throw new Exception("It's an exception.");
        }

        int[string] arr;
        try
        {
            arr["hello"] = func();
        }
        catch(Exception e)
        {
        }
        assert(arr.length == 0);
    }
    void bug7914()
    {
        size_t[ubyte] aa;
        aa[0] = aa.length;
        assert(aa[0] == 0);
    }
    void bug8070()
    {
        Object[string] arr;

        class A
        {
            this()
            {
                // at this point:
                assert("x" !in arr);
            }
        }

        arr["x"] = new A();
    }
    bug3825();
    bug5021();
    bug7914();
    bug8070();
}

void test3825x()
{
    return; // depends on AA implementation
    static int ctor, cpctor, dtor;

    static struct S
    {
        this(int)  { ++ctor; }
        this(this) { ++cpctor; }
        ~this()    { ++dtor; }
    }

    int[S] aa;
    {
        auto value = S(1);
        assert(ctor==1 && cpctor==0 && dtor==0);

        ref getRef(ref S s = value) { return s; }
        auto getVal() { return value; }

        aa[value] = 10;
        assert(ctor==1 && cpctor==1 && dtor==0);

        aa[getRef()] += 1;
        assert(ctor==1 && cpctor==1 && dtor==0);

        aa[getVal()] += 1;
        assert(ctor==1 && cpctor==2 && dtor==1);
    }
    assert(ctor==1 && cpctor==2 && dtor==2);
    assert(ctor + cpctor - aa.length == dtor);
}

/************************************************/
// https://issues.dlang.org/show_bug.cgi?id=10106

struct GcPolicy10106 {}

struct Uint24Array10106(SP = GcPolicy10106)
{
    this(this) {}
}

struct InversionList10106(SP = GcPolicy10106)
{
    Uint24Array10106!SP data;
}

alias InversionList10106!GcPolicy10106 CodepointSet10106;

struct PropertyTable10106
{
    CodepointSet10106[string] table;
}

/************************************************/

int main()
{
    testaa();
    test1899();
    test4523();
    test3825();
    test3825x();

    printf("Success\n");
    return 0;
}
