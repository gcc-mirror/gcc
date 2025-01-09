/+
TEST_OUTPUT:
---
fail_compilation/testOpApply.d(27): Error: `testOpApply.SameAttr.opApply` called with argument types `(int delegate(int i) pure nothrow @nogc @safe)` matches multiple overloads after qualifier conversion:
fail_compilation/testOpApply.d(13):     `testOpApply.SameAttr.opApply(int delegate(int) @system dg)`
and:
fail_compilation/testOpApply.d(18):     `testOpApply.SameAttr.opApply(int delegate(int) @system dg)`
---
+/

struct SameAttr
{
    int opApply(int delegate(int) @system dg) @system
    {
        return 0;
    }

    int opApply(int delegate(int) @system dg) @safe
    {
        return 0;
    }
}

void testSameAttr() @safe
{
    SameAttr sa;
    foreach (int i; sa) {}
}

/+
TEST_OUTPUT:
---
fail_compilation/testOpApply.d(104): Error: `testOpApply.SameAttr.opApply` called with argument types `(int delegate(int i) pure nothrow @nogc @system)` matches multiple overloads after qualifier conversion:
fail_compilation/testOpApply.d(13):     `testOpApply.SameAttr.opApply(int delegate(int) @system dg)`
and:
fail_compilation/testOpApply.d(18):     `testOpApply.SameAttr.opApply(int delegate(int) @system dg)`
---
+/
#line 100

void testSameAttr() @system
{
    SameAttr sa;
    foreach (int i; sa) {}
}

/+
TEST_OUTPUT:
---
fail_compilation/testOpApply.d(217): Error: `sa.opApply` matches more than one declaration:
fail_compilation/testOpApply.d(203):        `int(int delegate(int) dg)`
and:
fail_compilation/testOpApply.d(208):        `int(int delegate(string) dg)`
fail_compilation/testOpApply.d(217): Error: cannot uniquely infer `foreach` argument types
---
+/
#line 200

struct DifferentTypes
{
    int opApply(int delegate(int) dg)
    {
        return 0;
    }

    int opApply(int delegate(string) dg)
    {
        return 0;
    }
}

void testDifferentTypes()
{
    DifferentTypes sa;
    foreach (i; sa) {}
}

/+
TEST_OUTPUT:
---
fail_compilation/testOpApply.d(317): Error: `sa.opApply` matches more than one declaration:
fail_compilation/testOpApply.d(303):        `int(int delegate(int) dg)`
and:
fail_compilation/testOpApply.d(308):        `int(int delegate(long) dg)`
fail_compilation/testOpApply.d(317): Error: cannot uniquely infer `foreach` argument types
---
+/
#line 300

struct CovariantTypes
{
    int opApply(int delegate(int) dg)
    {
        return 0;
    }

    int opApply(int delegate(long) dg)
    {
        return 0;
    }
}

void testCovariantTypes()
{
    CovariantTypes sa;
    foreach (i; sa) {}
}

/+
See https://issues.dlang.org/show_bug.cgi?id=21683

TEST_OUTPUT:
---
fail_compilation/testOpApply.d(420): Error: `sa.opApply` matches more than one declaration:
fail_compilation/testOpApply.d(404):        `int(int delegate(int) dg)`
and:
fail_compilation/testOpApply.d(410):        `int(int delegate(ref int) dg)`
fail_compilation/testOpApply.d(420): Error: cannot uniquely infer `foreach` argument types
---
+/
#line 400

struct DifferentQualifiers
{
    int x;
    int opApply(int delegate(int) dg)
    {
        x = 1;
        return 0;
    }

    int opApply(int delegate(ref int) dg)
    {
        x = 2;
        return 0;
    }
}

void testDifferentQualifiers()
{
    DifferentQualifiers sa;
    foreach (i; sa) {}
}

/+
TEST_OUTPUT:
---
fail_compilation/testOpApply.d(504): Error: `sa.opApply` matches more than one declaration:
fail_compilation/testOpApply.d(404):        `int(int delegate(int) dg)`
and:
fail_compilation/testOpApply.d(410):        `int(int delegate(ref int) dg)`
fail_compilation/testOpApply.d(504): Error: cannot uniquely infer `foreach` argument types
---
+/
#line 500

void testDifferentQualifiersRef()
{
    DifferentQualifiers sa;
    foreach (ref i; sa) {}
}
