// https://issues.dlang.org/show_bug.cgi?id=4375: Dangling else
/*
TEST_OUTPUT:
---
fail_compilation/fail4375k.d-mixin-11(15): Error: else is dangling, add { } after condition at fail_compilation/fail4375k.d-mixin-11(12)
fail_compilation/fail4375k.d-mixin-11(12):        while parsing string mixin statement
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
