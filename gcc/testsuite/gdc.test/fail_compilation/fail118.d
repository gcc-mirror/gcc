/*
TEST_OUTPUT:
---
fail_compilation/fail118.d(43): Error: invalid `foreach` aggregate `Iter` of type `Iter`
fail_compilation/fail118.d(43):        maybe define `opApply()`, range primitives, or use `.tupleof`
fail_compilation/fail118.d(44): Error: invalid `foreach` aggregate `Iter` of type `Iter`
fail_compilation/fail118.d(44):        maybe define `opApply()`, range primitives, or use `.tupleof`
fail_compilation/fail118.d(47): Error: invalid `foreach` aggregate `s` of type `S*`
fail_compilation/fail118.d(49): Error: undefined identifier `unknown`
fail_compilation/fail118.d(37): Error: undefined identifier `doesNotExist`
fail_compilation/fail118.d(51): Error: template instance `fail118.error!()` error instantiating
fail_compilation/fail118.d(51): Error: invalid `foreach` aggregate `error()` of type `void`
---
*/

// https://issues.dlang.org/show_bug.cgi?id=441
// Crash on foreach of mixed-in aggregate.
template opHackedApply()
{
    struct Iter
    {
    }
}

class Foo
{
    mixin opHackedApply!() oldIterMix;
}

struct S
{
    int opApply(scope int delegate(const int) dg);
}

auto error()()
{
    doesNotExist();
}

void main()
{
    Foo f = new Foo;
    foreach (int i; f.oldIterMix.Iter) {}
    foreach (    i; f.oldIterMix.Iter) {}

    S* s;
    foreach (const i; s) {}

    foreach(const i; unknown) {}

    foreach (const i; error()) {}
}
