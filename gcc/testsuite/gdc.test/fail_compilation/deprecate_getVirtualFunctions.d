// REQUIRED_ARGS: -de

/*
TEST_OUTPUT:
---
fail_compilation/deprecate_getVirtualFunctions.d(18): Deprecation: `traits(isVirtualFunction)` is deprecated. Use `traits(isVirtualMethod)` instead
fail_compilation/deprecate_getVirtualFunctions.d(19): Deprecation: `traits(getVirtualFunctions)` is deprecated. Use `traits(getVirtualMethods)` instead
---
*/

class A
{
    void fun() {}
}

void main()
{
    auto a = __traits(isVirtualFunction, A.fun);
    foreach(f; __traits(getVirtualFunctions, A, "fun")) {}
}
