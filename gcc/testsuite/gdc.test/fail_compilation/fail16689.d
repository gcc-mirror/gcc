/*
TEST_OUTPUT:
---
fail_compilation/fail16689.d(3): Error: static assert:  "false"
fail_compilation/fail16689.d(6):        instantiated from here: `Issue16689!()`
---
*/
#line 1
mixin template Issue16689()
{
    static assert(false, "false");
}

mixin Issue16689!();
