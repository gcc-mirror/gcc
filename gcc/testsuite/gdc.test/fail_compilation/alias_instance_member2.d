/*
TEST_OUTPUT:
---
fail_compilation/alias_instance_member2.d(26): Error: cannot alias variable member `v` of variable `f`
fail_compilation/alias_instance_member2.d(26):        Use `typeof(f)` instead to preserve behaviour
fail_compilation/alias_instance_member2.d(30): Error: cannot alias function member `fun` of variable `f`
fail_compilation/alias_instance_member2.d(30):        Use `typeof(f)` instead to preserve behaviour
---
*/

@__edition_latest_do_not_use
module aim;

struct Foo
{
    int v;
    static int w;
    enum x = 5;
    void fun() {}
    static void gun() {}
}

struct Bar
{
    Foo f;
    alias v = f.v;
    alias v2 = typeof(f).v; // OK
    alias w = f.w; // OK
    alias x = f.x; // OK
    alias fun = f.fun;
    alias gun = f.gun; // OK
}
