// REQUIRED_ARGS: -w
// PERMUTE_ARGS: -debug

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=3882

/*
TEST_OUTPUT:
---
fail_compilation/fail3882.d(23): Warning: calling `fail3882.strictlyPure!int.strictlyPure` without side effects discards return value of type `int`; prepend a `cast(void)` if intentional
fail_compilation/fail3882.d(27): Warning: calling `fp` without side effects discards return value of type `int`; prepend a `cast(void)` if intentional
---
*/

@safe pure nothrow T strictlyPure(T)(T x)
{
    return x*x;
}

void main()
{
    int x = 3;
    strictlyPure(x);

    // https://issues.dlang.org/show_bug.cgi?id=12649
    auto fp = &strictlyPure!int;
    fp(x);
}

/******************************************/
// bugfix in TypeFunction::purityLevel

/*
TEST_OUTPUT:
---
fail_compilation/fail3882.d(48): Warning: calling `fail3882.f1` without side effects discards return value of type `int`; prepend a `cast(void)` if intentional
fail_compilation/fail3882.d(49): Warning: calling `fail3882.f2` without side effects discards return value of type `int`; prepend a `cast(void)` if intentional
Error: warnings are treated as errors
       Use -wi if you wish to treat warnings only as informational.
---
*/

nothrow pure int f1(immutable(int)[] a) { return 0; }
nothrow pure int f2(immutable(int)*  p) { return 0; }

void test_bug()
{
    f1([]);
    f2(null);
}
