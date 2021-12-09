// REQUIRED_ARGS: -w -unittest
// https://issues.dlang.org/show_bug.cgi?id=4375: Dangling else
/*
TEST_OUTPUT:
---
fail_compilation/fail4375t.d(16): Warning: else is dangling, add { } after condition at fail_compilation/fail4375t.d(13)
Error: warnings are treated as errors
       Use -wi if you wish to treat warnings only as informational.
---
*/

unittest {  // disallowed
    if (true)
        if (false)
            assert(52);
    else
        assert(53);
}

