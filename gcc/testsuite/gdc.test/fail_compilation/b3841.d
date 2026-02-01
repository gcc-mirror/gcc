// REQUIRED_ARGS: -de

/*
TEST_OUTPUT:
---
fail_compilation/b3841.d-mixin-45(45): Deprecation: `char += float` is performing truncating conversion
fail_compilation/b3841.d(69): Error: template instance `b3841.f!("+=", char, float)` error instantiating
fail_compilation/b3841.d-mixin-45(45): Deprecation: `int += float` is performing truncating conversion
fail_compilation/b3841.d(70): Error: template instance `b3841.f!("+=", int, float)` error instantiating
fail_compilation/b3841.d-mixin-45(45): Deprecation: `long += double` is performing truncating conversion
fail_compilation/b3841.d(71): Error: template instance `b3841.f!("+=", long, double)` error instantiating
fail_compilation/b3841.d-mixin-45(45): Deprecation: `char -= float` is performing truncating conversion
fail_compilation/b3841.d(69): Error: template instance `b3841.f!("-=", char, float)` error instantiating
fail_compilation/b3841.d-mixin-45(45): Deprecation: `int -= float` is performing truncating conversion
fail_compilation/b3841.d(70): Error: template instance `b3841.f!("-=", int, float)` error instantiating
fail_compilation/b3841.d-mixin-45(45): Deprecation: `long -= double` is performing truncating conversion
fail_compilation/b3841.d(71): Error: template instance `b3841.f!("-=", long, double)` error instantiating
fail_compilation/b3841.d-mixin-45(45): Deprecation: `char *= float` is performing truncating conversion
fail_compilation/b3841.d(69): Error: template instance `b3841.f!("*=", char, float)` error instantiating
fail_compilation/b3841.d-mixin-45(45): Deprecation: `int *= float` is performing truncating conversion
fail_compilation/b3841.d(70): Error: template instance `b3841.f!("*=", int, float)` error instantiating
fail_compilation/b3841.d-mixin-45(45): Deprecation: `long *= double` is performing truncating conversion
fail_compilation/b3841.d(71): Error: template instance `b3841.f!("*=", long, double)` error instantiating
fail_compilation/b3841.d-mixin-45(45): Deprecation: `char /= float` is performing truncating conversion
fail_compilation/b3841.d(69): Error: template instance `b3841.f!("/=", char, float)` error instantiating
fail_compilation/b3841.d-mixin-45(45): Deprecation: `int /= float` is performing truncating conversion
fail_compilation/b3841.d(70): Error: template instance `b3841.f!("/=", int, float)` error instantiating
fail_compilation/b3841.d-mixin-45(45): Deprecation: `long /= double` is performing truncating conversion
fail_compilation/b3841.d(71): Error: template instance `b3841.f!("/=", long, double)` error instantiating
fail_compilation/b3841.d-mixin-45(45): Deprecation: `char %= float` is performing truncating conversion
fail_compilation/b3841.d(69): Error: template instance `b3841.f!("%=", char, float)` error instantiating
fail_compilation/b3841.d-mixin-45(45): Deprecation: `int %= float` is performing truncating conversion
fail_compilation/b3841.d(70): Error: template instance `b3841.f!("%=", int, float)` error instantiating
fail_compilation/b3841.d-mixin-45(45): Deprecation: `long %= double` is performing truncating conversion
fail_compilation/b3841.d(71): Error: template instance `b3841.f!("%=", long, double)` error instantiating
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
