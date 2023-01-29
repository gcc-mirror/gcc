/*
TEST_OUTPUT:
---
fail_compilation/staticassertargsfail.d(10): Error: incompatible types for `('x') : (new Object)`: `char` and `object.Object`
fail_compilation/staticassertargsfail.d(10):        while evaluating `static assert` argument `['x', new Object] ~ ""`
---
*/


static assert(0, "abc", ['x', new Object] ~ "");
