// REQUIRED_ARGS: -w -o-

/*
TEST_OUTPUT:
---
fail_compilation/b3841.d-mixin-32(32): Warning: `char += float` is performing truncating conversion
fail_compilation/b3841.d-mixin-32(32): Warning: `int += float` is performing truncating conversion
fail_compilation/b3841.d-mixin-32(32): Warning: `long += double` is performing truncating conversion
fail_compilation/b3841.d-mixin-32(32): Warning: `char -= float` is performing truncating conversion
fail_compilation/b3841.d-mixin-32(32): Warning: `int -= float` is performing truncating conversion
fail_compilation/b3841.d-mixin-32(32): Warning: `long -= double` is performing truncating conversion
fail_compilation/b3841.d-mixin-32(32): Warning: `char *= float` is performing truncating conversion
fail_compilation/b3841.d-mixin-32(32): Warning: `int *= float` is performing truncating conversion
fail_compilation/b3841.d-mixin-32(32): Warning: `long *= double` is performing truncating conversion
fail_compilation/b3841.d-mixin-32(32): Warning: `char /= float` is performing truncating conversion
fail_compilation/b3841.d-mixin-32(32): Warning: `int /= float` is performing truncating conversion
fail_compilation/b3841.d-mixin-32(32): Warning: `long /= double` is performing truncating conversion
fail_compilation/b3841.d-mixin-32(32): Warning: `char %= float` is performing truncating conversion
fail_compilation/b3841.d-mixin-32(32): Warning: `int %= float` is performing truncating conversion
fail_compilation/b3841.d-mixin-32(32): Warning: `long %= double` is performing truncating conversion
Error: warnings are treated as errors
       Use -wi if you wish to treat warnings only as informational.
---
*/


void f(string op, LHS, RHS)()
{
    // pragma(msg, LHS, " += ", RHS);
    LHS a;
    RHS b;
    mixin("a "~op~" b;");
}

template Ops(T...)
{
    alias Ops = T;
}

void main()
{
    foreach (string op; Ops!("+=", "-=", "*=", "/=", "%="))
    {
        // OK
        f!(op, int, int)();
        f!(op, long, int)();
        f!(op, long, short)();
        f!(op, float, long)();
        f!(op, double, float)();
        
        // Should that really be OK ?
        f!(op, short, int)();
        f!(op, float, double)();

        // Not OK, truncating conversion.
        f!(op, char, float)();
        f!(op, int, float)();
        f!(op, long, double)();
    }

    // OK
    f!("^^=", int, int)();
    f!("^^=", long, int)();
    f!("^^=", long, short)();
    f!("^^=", float, long)();
    f!("^^=", double, float)();
    // Should that really be OK ?
    f!("^^=", float, double)();
}
