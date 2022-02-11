// https://issues.dlang.org/show_bug.cgi?id=11038
/*
TEST_OUTPUT:
---
fail_compilation/fail11038.d(16): Error: `printf` is not defined, perhaps `import core.stdc.stdio;` is needed?
---
*/

static
{
    import core.stdc.stdio;
}

void main()
{
    printf("foo");  // compiles
}
