// REQUIRED_ARGS: -w
// https://issues.dlang.org/show_bug.cgi?id=4375: Dangling else
/*
TEST_OUTPUT:
---
fail_compilation/fail4375s.d(19): Warning: else is dangling, add { } after condition at fail_compilation/fail4375s.d(13)
Error: warnings are treated as errors
       Use -wi if you wish to treat warnings only as informational.
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

