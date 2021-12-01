/* REQUIRED_ARGS: -o- -de
TEST_OUTPUT:
---
fail_compilation/test17586.d(13): Deprecation: `test17586.D.foo` is overriding the deprecated method `test17586.C.foo`
---
*/

class C{
    deprecated void foo(){}
}

class D : C{
    override void foo(){}
}
