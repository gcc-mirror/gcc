// REQUIRED_ARGS: -dw
// PERMUTE_ARGS:

/*
TEST_OUTPUT:
---
compilable/deprecate12979a.d(13): Deprecation: asm statement is assumed to throw - mark it with `nothrow` if it does not
---
*/

void foo() nothrow
{
    version(GNU)
    {
        asm
        {
            "";
        }
    }
    else
    {
        asm
        {
            ret;
        }
    }
}
