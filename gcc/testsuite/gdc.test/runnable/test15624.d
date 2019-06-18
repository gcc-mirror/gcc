/* PERMUTE_ARGS:
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
    return 0;
}
