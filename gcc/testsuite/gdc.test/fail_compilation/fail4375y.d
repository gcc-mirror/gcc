// REQUIRED_ARGS: -w
// https://issues.dlang.org/show_bug.cgi?id=4375: Dangling else
/*
TEST_OUTPUT:
---
fail_compilation/fail4375y.d(18): Warning: else is dangling, add { } after condition at fail_compilation/fail4375y.d(13)
Error: warnings are treated as errors
       Use -wi if you wish to treat warnings only as informational.
---
*/

static if (true)
    align(1)
        extern(C)
            pure
                static if (false)
                    void G10(){}
else
    void G11(){}

