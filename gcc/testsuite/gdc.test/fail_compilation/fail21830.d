/* REQUIRED_ARGS: -de -unittest
TEST_OUTPUT
---
fail_compilation/fail21830.d(24): Deprecation: struct `fail21830.OldS21830` is deprecated - Deprecated type
fail_compilation/fail21830.d(24): Deprecation: template `fail21830.test21830(T)(T t) if (is(T == OldS21830))` is deprecated - Deprecated template
fail_compilation/fail21830.d(24): Deprecation: struct `fail21830.OldS21830` is deprecated - Deprecated type
---
*/
#line 1
deprecated("Deprecated type")
struct OldS21830 { }

struct NewS21830 { }

static if (1)
{
    auto test21830(T)(T t)
    if (is(T == NewS21830))
    {
        return T.init;
    }
}

deprecated("Deprecated template")
auto test21830(T)(T t)
if (is(T == OldS21830))
{
    return T.init;
}

unittest
{
    auto b = test21830(OldS21830());
}
