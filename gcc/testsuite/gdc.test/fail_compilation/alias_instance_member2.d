/*
TEST_OUTPUT:
---
fail_compilation/alias_instance_member2.d(20): Error: cannot alias member of variable `f`
fail_compilation/alias_instance_member2.d(20):        Use `typeof(f)` instead to preserve behaviour
---
*/

@__edition_latest_do_not_use
module aim;

struct Foo
{
    int v;
}

struct Bar
{
    Foo f;
    alias v = f.v;
}
