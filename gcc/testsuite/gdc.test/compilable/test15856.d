// REQUIRED_ARGS: -transition=checkimports -de
// PERMUTE_ARGS:
/*
TEST_PUTPUT:
---
---
*/

class Foo
{
    import imports.a15856;

    struct Bar
    {
        c_long a;
    }
}
