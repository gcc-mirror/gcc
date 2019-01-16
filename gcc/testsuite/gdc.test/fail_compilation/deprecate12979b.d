// REQUIRED_ARGS: -de
// PERMUTE_ARGS:

/*
TEST_OUTPUT:
---
fail_compilation/deprecate12979b.d(13): Deprecation: asm statement is assumed to be impure - mark it with 'pure' if it is not
---
*/

void foo() pure
{
    asm
    {
        ret;
    }
}
