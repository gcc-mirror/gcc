/*
TEST_OUTPUT:
---
fail_compilation/fail15068.d(17): Error: `T!int` is not a valid template instance, because `T` is not a template declaration but a type (`T == int`)
fail_compilation/fail15068.d(13): Error: template instance `fail15068.Stuff!int` error instantiating
---
*/

// https://issues.dlang.org/show_bug.cgi?id=15068

void main()
{
    Stuff!int s;
}
struct Stuff(T)
{
    T!int var;
}
