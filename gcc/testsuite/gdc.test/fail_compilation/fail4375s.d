// REQUIRED_ARGS: -w
// https://issues.dlang.org/show_bug.cgi?id=4375: Dangling else
/*
TEST_OUTPUT:
---
fail_compilation/fail4375s.d(17): Warning: else is dangling, add { } after condition at fail_compilation/fail4375s.d(11)
---
*/

void main() {
    if (true)
        try
            assert(106);
        catch(Exception e)
            if (true)
                assert(107);
    else
        assert(108);
}

