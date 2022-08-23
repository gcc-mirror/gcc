// REQUIRED_ARGS: -w
// https://issues.dlang.org/show_bug.cgi?id=4375: Dangling else
/*
TEST_OUTPUT:
---
fail_compilation/fail4375c.d(17): Warning: else is dangling, add { } after condition at fail_compilation/fail4375c.d(13)
Error: warnings are treated as errors
       Use -wi if you wish to treat warnings only as informational.
---
*/

void main() {
    if (true)
        if (false) {
            assert(6.1);
        }
    else {
        assert(6.2);
    }
}
