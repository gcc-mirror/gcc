/*
TEST_OUTPUT:
---
fail_compilation/fail118.d(26): Error: invalid foreach aggregate `Iter`, define opApply(), range primitives, or use .tupleof
fail_compilation/fail118.d(27): Error: invalid foreach aggregate `Iter`, define opApply(), range primitives, or use .tupleof
---
*/

// Issue 441 - Crash on foreach of mixed-in aggregate.

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

void main()
{
    Foo f = new Foo;
    foreach (int i; f.oldIterMix.Iter) {}
    foreach (    i; f.oldIterMix.Iter) {}
}
