/* REQUIRED_ARGS: -de -unittest
TEST_OUTPUT
---
fail_compilation/fail21831.d(19): Deprecation: struct `fail21831.S21831` is deprecated - Deprecated type
fail_compilation/fail21831.d(19): Deprecation: template `fail21831.test21831(T)(T t) if (__traits(isDeprecated, T))` is deprecated - Deprecated template
fail_compilation/fail21831.d(19): Deprecation: struct `fail21831.S21831` is deprecated - Deprecated type
---
*/
#line 1
deprecated("Deprecated type")
struct S21831 { }

auto test21831(T)(T t)
if (!__traits(isDeprecated, T))
{
    return T.init;
}

deprecated("Deprecated template")
auto test21831(T)(T t)
if (__traits(isDeprecated, T))
{
    return T.init;
}

unittest
{
    auto b = test21831(S21831());
}
