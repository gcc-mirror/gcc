// https://issues.dlang.org/show_bug.cgi?id=4375: Dangling else
/*
TEST_OUTPUT:
---
fail_compilation/fail4375r.d(16): Error: else is dangling, add { } after condition at fail_compilation/fail4375r.d(10)
---
*/

void main() {
    if (true)
        try
            assert(103);
        finally
            if (true)
                assert(104);
    else
        assert(105);
}
