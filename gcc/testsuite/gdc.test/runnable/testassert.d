/*
REQUIRED_ARGS: -checkaction=context -preview=dip1000
PERMUTE_ARGS: -O -g -inline
*/

void test8765()
{
    string msg;
    try
    {
        int a = 0;
        assert(a);
    }
    catch (Throwable e)
    {
        // no-message -> assert expression
        msg = e.msg;
    }
    assert(msg == "0 != true");
}

 void test9255()
{
    string file;
    try
    {
        int x = 0;
        assert(x);
    }
    catch (Throwable e)
    {
        file = e.file;
    }

    version(Windows)
        assert(file && file == r"runnable\testassert.d");
    else
        assert(file && file == "runnable/testassert.d");
}

// https://issues.dlang.org/show_bug.cgi?id=20114
void test20114()
{
    // Function call returning simple type
    static int fun() {
        static int i = 0;
        assert(i++ == 0);
        return 3;
    }

    const a = getMessage(assert(fun() == 4));
    assert(a == "3 != 4");

    // Function call returning complex type with opEquals
    static struct S
    {
        bool opEquals(const int x) const
        {
            return false;
        }
    }

    static S bar()
    {
        static int i = 0;
        assert(i++ == 0);
        return S.init;
    }

    const b = getMessage(assert(bar() == 4));
    assert(b == "S() != 4");

    // Non-call expression with side effects
    int i = 0;
    const c = getMessage(assert(++i == 0));
    assert(c == "1 != 0");
}

version (DigitalMars) version (Win64) version = DMD_Win64;

void test20375() @safe
{
    static struct RefCounted
    {
        // Force temporary through "impure" generator function
        static RefCounted create() @trusted
        {
            __gshared int counter = 0;
            return RefCounted(++counter > 0);
        }

        static int instances;
        static int postblits;

        this(bool) @safe
        {
            instances++;
        }

        this(this) @safe
        {
            instances++;
            postblits++;
        }

        ~this() @safe
        {
            // make the dtor non-nothrow (we are tracking clean-ups during AssertError unwinding)
            if (postblits > 100)
                throw new Exception("");
            assert(instances > 0);
            instances--;
        }

        bool opEquals(RefCounted) @safe
        {
            return true;
        }
    }

    {
        auto a = RefCounted.create();
        RefCounted.instances++; // we're about to construct an instance below, increment manually
        assert(a == RefCounted()); // both operands are pure expressions => no temporaries
    }

    assert(RefCounted.instances == 0);
    assert(RefCounted.postblits == 0);

    {
        auto a = RefCounted.create();
        assert(a == RefCounted.create()); // impure rhs is promoted to a temporary lvalue => copy for a.opEquals(rhs)
    }

    assert(RefCounted.instances == 0);
    assert(RefCounted.postblits == 1);
    RefCounted.postblits = 0;

    {
        const msg = getMessage(assert(RefCounted.create() != RefCounted.create())); // both operands promoted to temporary lvalues
        assert(msg == "RefCounted() == RefCounted()");
    }

    version (DMD_Win64) // FIXME: temporaries apparently not destructed when unwinding via AssertError
    {
        assert(RefCounted.instances >= 0 && RefCounted.instances <= 2);
        RefCounted.instances = 0;
    }
    else
        assert(RefCounted.instances == 0);
    assert(RefCounted.postblits == 1);
    RefCounted.postblits = 0;

    static int numGetLvalImpureCalls = 0;
    ref RefCounted getLvalImpure() @trusted
    {
        numGetLvalImpureCalls++;
        __gshared lval = RefCounted(); // not incrementing RefCounted.instances
        return lval;
    }

    {
        const msg = getMessage(assert(getLvalImpure() != getLvalImpure())); // both operands promoted to ref temporaries
        assert(msg == "RefCounted() == RefCounted()");
    }

    assert(numGetLvalImpureCalls == 2);
    assert(RefCounted.instances == 0);
    assert(RefCounted.postblits == 1);
    RefCounted.postblits = 0;
}

// https://issues.dlang.org/show_bug.cgi?id=21471
void test21471()
{
    {
        static struct S
        {
            S get()() const { return this; }
        }

        static auto f(S s) { return s; }

        auto s = S();
        assert(f(s.get) == s);
    }
    {
        pragma(inline, true)
        real exp(real x) pure nothrow
        {
            return x;
        }

        bool isClose(int lhs, real rhs) pure nothrow
        {
            return false;
        }
        auto c = 0;
        assert(!isClose(c, exp(1)));
    }
}

// https://issues.dlang.org/show_bug.cgi?id=20581
void test20581() @safe
{
    static auto retro(return scope int[] s) @safe
    {
        static struct R
        {
            int[] source;
        }
        return R(s);
    }

    int[5] a = [ 1, 2, 3, 4, 5 ];
    // Creates ref temporary __assertTmpXXXX for source
    // Error: address of variable a assigned to __assertOp27 with longer lifetime
    assert(retro(a[]).source is a[]);
}

string getMessage(T)(lazy T expr) @trusted
{
    try
    {
        expr();
        return null;
    }
    catch (Throwable t)
    {
        return t.msg;
    }
}

void testMixinExpression() @safe
{
    static struct S
    {
        bool opEquals(S) @safe { return true; }
    }

    const msg = getMessage(assert(mixin("S() != S()")));
    assert(msg == "S() == S()");
}

void testUnaryFormat()
{
    int zero = 0, one = 1;
    assert(getMessage(assert(zero)) == "0 != true");
    assert(getMessage(assert(!one)) == "1 == true");

    assert(getMessage(assert(!cast(int) 1.5)) == "1 == true");

    assert(getMessage(assert(!!__ctfe)) == "assert(__ctfe) failed!");

    static struct S
    {
        int i;
        bool opCast() const
        {
            return i < 2;
        }
    }

    assert(getMessage(assert(S(4))) == "S(4) != true");

    S s = S(4);
    assert(getMessage(assert(*&s)) == "S(4) != true");

    assert(getMessage(assert(--(++zero))) == "0 != true");
}

void testAssignments()
{
    int a = 1;
    int b = 2;
    assert(getMessage(assert(a -= --b)) == "0 != true");

    static ref int c()
    {
        static int counter;
        counter++;
        return counter;
    }

    assert(getMessage(assert(--c())) == "0 != true");
}

/// https://issues.dlang.org/show_bug.cgi?id=21472
void testTupleFormat()
{
    alias AliasSeq(T...) = T;

    // Default usage
    {
        alias a = AliasSeq!(1, "ab");
        alias b = AliasSeq!(false, "xyz");
        assert(getMessage(assert(a == b)) == `(1, "ab") != (false, "xyz")`);
    }

    // Single elements work but are not formatted as tuples
    // Is this acceptable? (a workaround would probably require a
    // different name for the tuple formatting hook)
    {
        alias a = AliasSeq!(1, "ab", []);
        alias b = AliasSeq!(false, "xyz", [1]);
        assert(getMessage(assert(a == b)) == `(1, "ab", []) != (false, "xyz", [1])`);
    }

    // Also works with tupleof (as taken from the bug report)
    {
        static struct Var { int a, b; }
        const a = Var(1, 2);
        const b = Var(3, 4);
        const msg = getMessage(assert(a.tupleof == b.tupleof));
        assert(msg == `(1, 2) != (3, 4)`);
    }

    // Also works when creating temporaries for the TupleExp
    {
        static struct S
        {
            int a, b;
        }

        static S get()
        {
            static int i;
            return S(i++, i++);
        }

        auto a = get().tupleof;
        auto b = get().tupleof;

        string msg = getMessage(assert(a == b));
        assert(msg == `(0, 1) != (2, 3)`);

        msg = getMessage(assert(get().tupleof == AliasSeq!(2, 3)));
        assert(msg == `(4, 5) != (2, 3)`);

        msg = getMessage(assert(get().tupleof == get().tupleof));
        assert(msg == `(6, 7) != (8, 9)`);
    }
}

// https://issues.dlang.org/show_bug.cgi?id=21682
void testStaticOperators()
{
    static class environment {
        static bool opCmp(scope const(char)[] name)
        {
            return false;
        }

        static bool opBinaryRight(string op : "in")(scope const(char)[] name)
        {
            return false;
        }
    }

    string msg = getMessage(assert(environment < "Hello"));
    assert(msg == `"environment" >= "Hello"`);

    msg = getMessage(assert("Hello" in environment));
    assert(msg == `"Hello" !in "environment"`);
}

void main()
{
    test8765();
    test9255();
    test20114();
    test20375();
    test21471();
    test20581();
    testMixinExpression();
    testUnaryFormat();
    testAssignments();
    testTupleFormat();
    testStaticOperators();
}
