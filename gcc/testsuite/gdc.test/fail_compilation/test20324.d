/*
REQUIRED_ARGS: -unittest
TEST_OUTPUT:
---
fail_compilation/test20324.d(16): Error: argument `Test()` to __traits(getUnitTests) must be a module or aggregate, not a template
fail_compilation/test20324.d(16):        while evaluating `pragma(msg, __traits(getUnitTests, Test))`
---
*/
// https://issues.dlang.org/show_bug.cgi?id=20324

template Test() {
    unittest {
    }
}

pragma(msg, __traits(getUnitTests, Test));
