// REQUIRED_ARGS: -w
// https://issues.dlang.org/show_bug.cgi?id=4375: Dangling else
/*
TEST_OUTPUT:
---
fail_compilation/fail4375q.d(17): Warning: else is dangling, add { } after condition at fail_compilation/fail4375q.d(13)
fail_compilation/fail4375q.d(14): Error: `with` expressions must be aggregate types or pointers to them, not `int`
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
