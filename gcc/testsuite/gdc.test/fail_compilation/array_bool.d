/*
REQUIRED_ARGS: -de
TEST_OUTPUT:
---
fail_compilation/array_bool.d(13): Deprecation: assert condition cannot be a string literal
fail_compilation/array_bool.d(13):        If intentional, use `"foo" !is null` instead to preserve behaviour
fail_compilation/array_bool.d(14): Deprecation: static assert condition cannot be a string literal
fail_compilation/array_bool.d(14):        If intentional, use `"foo" !is null` instead to preserve behaviour
---
*/
void main()
{
    assert("foo");
    static assert("foo");

    assert("foo".ptr); // OK
    static assert("foo".ptr); // OK

    enum e = "bar";
    static assert(e); // OK
    assert(e); // OK
}
