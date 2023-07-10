/*
REQUIRED_ARGS: -unittest
TEST_OUTPUT:
---
AliasSeq!(__unittest_L14_C5_1, __unittest_L14_C5_2)
AliasSeq!(__unittest_L14_C5_2)
---
*/
// https://issues.dlang.org/show_bug.cgi?id=21330

module test21330;

mixin template Test() {
    unittest {
    }
}

mixin Test;
mixin Test tm;

pragma(msg, __traits(getUnitTests, test21330));
pragma(msg, __traits(getUnitTests, tm));
