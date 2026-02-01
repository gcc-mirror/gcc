// https://issues.dlang.org/show_bug.cgi?id=4375: Dangling else
/*
TEST_OUTPUT:
---
fail_compilation/fail4375f.d(13): Error: else is dangling, add { } after condition at fail_compilation/fail4375f.d(10)
---
*/

void main() {
    version (A)
        version (B)
            assert(25.1);
    else
        assert(25.2);
}
