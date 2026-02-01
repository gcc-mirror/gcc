// https://issues.dlang.org/show_bug.cgi?id=4375: Dangling else
/*
TEST_OUTPUT:
---
fail_compilation/fail4375y.d(15): Error: else is dangling, add { } after condition at fail_compilation/fail4375y.d(10)
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
