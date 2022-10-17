/*
TEST_OUTPUT:
---
fail_compilation/fail20551.d(15): Error: cannot take address of lazy parameter `e` in `@safe` function `opAssign`
fail_compilation/fail20551.d(26): Error: template instance `fail20551.LazyStore!int.LazyStore.opAssign!int` error instantiating
---
*/

struct LazyStore(T)
{
    T delegate() @safe dg;

    void opAssign(E)(lazy E e) @safe
    {
        dg = cast(typeof(dg)) &e;
    }

    T test() @safe{ return dg(); }
}

static LazyStore!int f;

void main(string[] args) @safe
{
    int x = 1;
    f = x + x + 20 + x * 20;
}
