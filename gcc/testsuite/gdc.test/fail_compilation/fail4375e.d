// REQUIRED_ARGS: -w
// https://issues.dlang.org/show_bug.cgi?id=4375: Dangling else
/*
TEST_OUTPUT:
---
fail_compilation/fail4375e.d(14): Warning: else is dangling, add { } after condition at fail_compilation/fail4375e.d(11)
---
*/

void main() {
    version (A)
        if (true)
            assert(24);
    else
        assert(25);
}

