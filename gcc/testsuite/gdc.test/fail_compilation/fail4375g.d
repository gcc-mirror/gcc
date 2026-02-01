// https://issues.dlang.org/show_bug.cgi?id=4375: Dangling else
/*
TEST_OUTPUT:
---
fail_compilation/fail4375g.d(13): Error: else is dangling, add { } after condition at fail_compilation/fail4375g.d(10)
---
*/

void main() {
    static if (true)
        static if (true)
            assert(33);
    else
        assert(34);
}
