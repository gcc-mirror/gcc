// https://issues.dlang.org/show_bug.cgi?id=11038
/*
TEST_OUTPUT:
---
fail_compilation/fail11038.d(16): Error: `writeln` is not defined, perhaps `import std.stdio;` is needed?
---
*/

static
{
    import std.stdio;
}

void main()
{
    writeln("foo");  // compiles
}
