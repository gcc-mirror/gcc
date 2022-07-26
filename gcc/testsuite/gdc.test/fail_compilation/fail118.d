/*
TEST_OUTPUT:
---
fail_compilation/fail118.d(45): Error: invalid `foreach` aggregate `Iter` of type `Iter`
fail_compilation/fail118.d(45):        `foreach` works with input ranges (implementing `front` and `popFront`), aggregates implementing `opApply`, or the result of an aggregate's `.tupleof` property
fail_compilation/fail118.d(45):        https://dlang.org/phobos/std_range_primitives.html#isInputRange
fail_compilation/fail118.d(46): Error: invalid `foreach` aggregate `Iter` of type `Iter`
fail_compilation/fail118.d(46):        `foreach` works with input ranges (implementing `front` and `popFront`), aggregates implementing `opApply`, or the result of an aggregate's `.tupleof` property
fail_compilation/fail118.d(46):        https://dlang.org/phobos/std_range_primitives.html#isInputRange
fail_compilation/fail118.d(49): Error: invalid `foreach` aggregate `s` of type `S*`
fail_compilation/fail118.d(51): Error: undefined identifier `unknown`
fail_compilation/fail118.d(39): Error: undefined identifier `doesNotExist`
fail_compilation/fail118.d(53): Error: template instance `fail118.error!()` error instantiating
fail_compilation/fail118.d(53): Error: invalid `foreach` aggregate `error()` of type `void`
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
