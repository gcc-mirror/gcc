// REQUIRED_ARGS: -w
// https://issues.dlang.org/show_bug.cgi?id=4375: Dangling else
/*
TEST_OUTPUT:
---
fail_compilation/fail4375k.d-mixin-11(15): Warning: else is dangling, add { } after condition at fail_compilation/fail4375k.d-mixin-11(12)
---
*/

void main() {
    mixin(q{
        if(true)
            if(true)
                assert(54);
        else
            assert(55);
    });
}

