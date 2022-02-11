// REQUIRED_ARGS: -w
// https://issues.dlang.org/show_bug.cgi?id=4375: Dangling else
/*
TEST_OUTPUT:
---
fail_compilation/fail4375o.d(17): Warning: else is dangling, add { } after condition at fail_compilation/fail4375o.d(13)
Error: warnings are treated as errors
       Use -wi if you wish to treat warnings only as informational.
---
*/

void main() {
    if (true)
        for (; false;)
            if (true)
                assert(82);
    else
        assert(83);
}

