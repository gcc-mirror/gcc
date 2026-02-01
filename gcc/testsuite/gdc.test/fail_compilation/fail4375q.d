// https://issues.dlang.org/show_bug.cgi?id=4375: Dangling else
/*
TEST_OUTPUT:
---
fail_compilation/fail4375q.d(15): Error: else is dangling, add { } after condition at fail_compilation/fail4375q.d(11)
---
*/

void main() {
    auto x = 1;
    if (true)
        with (x)
            if (false)
                assert(90);
    else
        assert(91);
}
