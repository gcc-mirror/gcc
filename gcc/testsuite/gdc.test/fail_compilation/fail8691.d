/*
TEST_OUTPUT:
---
fail_compilation/fail8691.d(7): Error: struct `fail8691.Foo` cannot have field `f` with static array of same struct type
---
*/
struct Foo
{
    Foo[1] f;
}
