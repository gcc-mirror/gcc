// REQUIRED_ARGS: -w
// https://issues.dlang.org/show_bug.cgi?id=4375: Dangling else
/*
TEST_OUTPUT:
---
fail_compilation/fail4375m.d(15): Warning: else is dangling, add { } after condition at fail_compilation/fail4375m.d(12)
---
*/

void main() {
    do
        if (true)
            if (true)
                assert(76);
        else
            assert(77);
    while (false);
}

