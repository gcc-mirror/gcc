/* REQUIRED_ARGS: -preview=dip1000
   TEST_OUTPUT:
---
fail_compilation/test14238.d(20): Error: scope parameter `fn` may not be returned
fail_compilation/test14238.d(28): Error: escaping reference to stack allocated value returned by `&baz`
---
*/
// https://issues.dlang.org/show_bug.cgi?id=14238

@safe:

alias Fn = ref int delegate() return;

ref int foo(return scope Fn fn)
{
    return fn(); // Ok
}

ref int foo2(scope Fn fn) {
    return fn(); // Error
}

ref int bar() {
    int x;
    ref int baz() {
            return x;
    }
    return foo(&baz);
}
