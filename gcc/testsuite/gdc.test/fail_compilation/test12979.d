// PERMUTE_ARGS:

/*
TEST_OUTPUT:
---
fail_compilation/test12979.d(13): Error: const/immutable/shared/inout attributes are not allowed on `asm` blocks
---
*/

void foo()
{
    asm const shared
    {
        ret;
    }
}
