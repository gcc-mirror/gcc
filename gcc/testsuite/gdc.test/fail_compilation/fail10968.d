/*
TEST_OUTPUT:
---
fail_compilation/fail10968.d(33): Error: pure function 'fail10968.bar' cannot call impure function 'fail10968.SA.__postblit'
fail_compilation/fail10968.d(33): Error: @safe function 'fail10968.bar' cannot call @system function 'fail10968.SA.__postblit'
fail_compilation/fail10968.d(34): Error: pure function 'fail10968.bar' cannot call impure function 'fail10968.SA.__postblit'
fail_compilation/fail10968.d(34): Error: @safe function 'fail10968.bar' cannot call @system function 'fail10968.SA.__postblit'
fail_compilation/fail10968.d(35): Error: pure function 'fail10968.bar' cannot call impure function 'fail10968.SA.__postblit'
fail_compilation/fail10968.d(35): Error: @safe function 'fail10968.bar' cannot call @system function 'fail10968.SA.__postblit'
fail_compilation/fail10968.d(38): Error: pure function 'fail10968.bar' cannot call impure function 'fail10968.SA.__postblit'
fail_compilation/fail10968.d(38): Error: @safe function 'fail10968.bar' cannot call @system function 'fail10968.SA.__postblit'
fail_compilation/fail10968.d(39): Error: pure function 'fail10968.bar' cannot call impure function 'fail10968.SA.__postblit'
fail_compilation/fail10968.d(39): Error: @safe function 'fail10968.bar' cannot call @system function 'fail10968.SA.__postblit'
fail_compilation/fail10968.d(40): Error: pure function 'fail10968.bar' cannot call impure function 'fail10968.SA.__postblit'
fail_compilation/fail10968.d(40): Error: @safe function 'fail10968.bar' cannot call @system function 'fail10968.SA.__postblit'
---
*/

struct SA
{
    this(this)
    {
        throw new Exception("BOOM!");
    }
}

void bar() pure @safe
{
    SA    ss;
    SA[1] sa;

    // TOKassign
    ss = ss;
    sa = ss;
    sa = sa;

    // TOKconstruct
    SA    ss2 = ss;
    SA[1] sa2 = ss;
    SA[1] sa3 = sa;
}

/*
TEST_OUTPUT:
---
fail_compilation/fail10968.d(66): Error: struct fail10968.SD is not copyable because it is annotated with `@disable`
fail_compilation/fail10968.d(67): Error: struct fail10968.SD is not copyable because it is annotated with `@disable`
fail_compilation/fail10968.d(68): Error: struct fail10968.SD is not copyable because it is annotated with `@disable`
fail_compilation/fail10968.d(71): Error: struct fail10968.SD is not copyable because it is annotated with `@disable`
fail_compilation/fail10968.d(72): Error: struct fail10968.SD is not copyable because it is annotated with `@disable`
fail_compilation/fail10968.d(73): Error: struct fail10968.SD is not copyable because it is annotated with `@disable`
---
*/

struct SD
{
    this(this) @disable;
}

void baz()
{
    SD    ss;
    SD[1] sa;

    // TOKassign
    ss = ss;
    sa = ss;
    sa = sa;

    // TOKconstruct
    SD    ss2 = ss;
    SD[1] sa2 = ss;
    SD[1] sa3 = sa;
}
