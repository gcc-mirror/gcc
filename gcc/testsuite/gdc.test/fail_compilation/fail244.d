// REQUIRED_ARGS: -de
/*
TEST_OUTPUT:
---
fail_compilation/fail244.d(27): Deprecation: variable `fail244.StructWithDeps.value` is deprecated
fail_compilation/fail244.d(28): Deprecation: variable `fail244.StructWithDeps.value` is deprecated
fail_compilation/fail244.d(29): Deprecation: variable `fail244.StructWithDeps.value` is deprecated
fail_compilation/fail244.d(30): Deprecation: variable `fail244.StructWithDeps.value` is deprecated
fail_compilation/fail244.d(32): Deprecation: variable `fail244.StructWithDeps.staticValue` is deprecated
fail_compilation/fail244.d(33): Deprecation: variable `fail244.StructWithDeps.staticValue` is deprecated
fail_compilation/fail244.d(34): Deprecation: variable `fail244.StructWithDeps.staticValue` is deprecated
fail_compilation/fail244.d(35): Deprecation: variable `fail244.StructWithDeps.staticValue` is deprecated
fail_compilation/fail244.d(36): Deprecation: variable `fail244.StructWithDeps.staticValue` is deprecated
fail_compilation/fail244.d(37): Deprecation: variable `fail244.StructWithDeps.staticValue` is deprecated
---
*/

//import std.stdio;

struct StructWithDeps
{
    deprecated int value;
    deprecated static int staticValue;

    void test(StructWithDeps obj)
    {
        obj.value = 666;
        this.value = 666;
        auto n1 = obj.value;
        auto n2 = this.value;

        obj.staticValue = 102;
        this.staticValue = 103;
        StructWithDeps.staticValue = 104;
        auto n3 = obj.staticValue;
        auto n4 = this.staticValue;
        auto n5 = StructWithDeps.staticValue;
    }
}
