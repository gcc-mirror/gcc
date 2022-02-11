/* PERMUTE_ARGS: -preview=dip1000
 */

// https://issues.dlang.org/show_bug.cgi?id=15624

struct Foo {
        int x;
        int opApply(int delegate(int, string, string) @safe dg) @safe {
                x = 1;
                return 0;
        }
        int opApply(int delegate(int, string, string) @system dg) @system {
                x = 2;
                return 0;
        }
}

void testSafe() @safe {
        Foo foo;
        foreach (i, k, v; foo) {
        }
        assert(foo.x == 1);
}

void testSystem() @system {
        Foo foo;
        foreach (i, k, v; foo) {
        }
        assert(foo.x == 2);
}

void test() @system
{
    Foo f;

    int dgsafe  (int x, string s, string t) @safe   { return 1; }
    int dgsystem(int x, string s, string t) @system { return 1; }

    f.opApply(&dgsafe);
    assert(f.x == 1);
    f.opApply(&dgsystem);
    assert(f.x == 2);
}

int main()
{
    testSafe();
    testSystem();
    test();
    testDifferentTypes();
    testSameAttributes();
    testInverseAttributes();
    return 0;
}

void testDifferentTypes()
{
    static struct DifferentTypes
    {
        int x;
        int opApply(int delegate(int) dg) @safe {
            x = 1;
            return 0;
        }
        int opApply(int delegate(long) dg) @safe {
            x = 2;
            return 0;
        }
    }

    DifferentTypes dt;
    foreach (int i; dt) {}
    assert(dt.x == 1);

    foreach (long i; dt) {}
    assert(dt.x == 2);
}

void testSameAttributes()
{
    static struct SameAttributes
    {
        int x;
        int opApply(int delegate(int) @system dg) @safe {
            x = 1;
            return 0;
        }
        int opApply(int delegate(int) @safe dg) @safe {
            x = 2;
            return 0;
        }
    }

    static void safe() @safe
    {
        SameAttributes sa;
        foreach (i; sa) {}
        assert(sa.x == 2);
    }
    safe();

    static void system() @system
    {
        SameAttributes sa;
        foreach (i; sa) {}
        assert(sa.x == 1);
    }
    system();
}

// Not useful but enabled by the associated patch
void testInverseAttributes()
{
    static struct InverseAttributes
    {
        int x;
        int opApply(int delegate(int) @system dg) @safe {
            x = 1;
            return 0;
        }
        int opApply(int delegate(int) @safe dg) @system {
            x = 2;
            return 0;
        }
    }

    static void system() @system
    {
        InverseAttributes sa;
        foreach (i; sa) {}
        assert(sa.x == 1);
    }
    system();

    static void safe() @safe
    {
        InverseAttributes sa;
        (() @trusted { foreach (i; sa) {} })();
        assert(sa.x == 2);
    }
    safe();
}

// https://issues.dlang.org/show_bug.cgi?id=20907
Lockstep!() lockstep()
{
    return Lockstep!()();
}

struct Lockstep()
{
    int opApply(int delegate(int) callback) @system
    {
        return 0;
    }

    int opApply(int delegate(int) pure nothrow @nogc @safe callback) pure nothrow @nogc @safe
    {
        return 0;
    }
}

void foo0()
{
    foreach (x; lockstep()) {}
}

void foo1()
{
    foreach (x; lockstep()) {}
}
