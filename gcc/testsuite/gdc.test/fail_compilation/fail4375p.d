// https://issues.dlang.org/show_bug.cgi?id=4375: Dangling else
/*
TEST_OUTPUT:
---
fail_compilation/fail4375p.d(17): Error: else is dangling, add { } after condition at fail_compilation/fail4375p.d(10)
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
