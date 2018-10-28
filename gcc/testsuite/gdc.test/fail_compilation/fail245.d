// REQUIRED_ARGS: -de
/*
TEST_OUTPUT:
---
fail_compilation/fail245.d(27): Deprecation: variable fail245.ClassWithDeps.value is deprecated
fail_compilation/fail245.d(28): Deprecation: variable fail245.ClassWithDeps.value is deprecated
fail_compilation/fail245.d(29): Deprecation: variable fail245.ClassWithDeps.value is deprecated
fail_compilation/fail245.d(30): Deprecation: variable fail245.ClassWithDeps.value is deprecated
fail_compilation/fail245.d(32): Deprecation: variable fail245.ClassWithDeps.staticValue is deprecated
fail_compilation/fail245.d(33): Deprecation: variable fail245.ClassWithDeps.staticValue is deprecated
fail_compilation/fail245.d(34): Deprecation: variable fail245.ClassWithDeps.staticValue is deprecated
fail_compilation/fail245.d(35): Deprecation: variable fail245.ClassWithDeps.staticValue is deprecated
fail_compilation/fail245.d(36): Deprecation: variable fail245.ClassWithDeps.staticValue is deprecated
fail_compilation/fail245.d(37): Deprecation: variable fail245.ClassWithDeps.staticValue is deprecated
---
*/

//import std.stdio;

class ClassWithDeps
{
    deprecated int value;
    deprecated static int staticValue;

    void test(ClassWithDeps obj)
    {
        obj.value = 666;
        this.value = 666;
        auto n1 = obj.value;
        auto n2 = this.value;

        obj.staticValue = 102;
        this.staticValue = 103;
        ClassWithDeps.staticValue = 104;
        auto n3 = obj.staticValue;
        auto n4 = this.staticValue;
        auto n5 = ClassWithDeps.staticValue;
    }
}
