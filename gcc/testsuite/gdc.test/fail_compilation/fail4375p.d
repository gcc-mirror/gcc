// REQUIRED_ARGS: -w
// https://issues.dlang.org/show_bug.cgi?id=4375: Dangling else
/*
TEST_OUTPUT:
---
fail_compilation/fail4375p.d(19): Warning: else is dangling, add { } after condition at fail_compilation/fail4375p.d(12)
fail_compilation/fail4375p.d(16): Error: undefined identifier `x`
---
*/

void main() {
    if (true)
        while (false)
            for (;;)
                scope (exit)
                    synchronized (x)
                        if (true)
                            assert(90);
    else
        assert(89);
}

