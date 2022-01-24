// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=94424
// { dg-additional-options "-fmain -funittest" }
// { dg-do run }
// { dg-skip-if "needs gcc/config.d" { ! d_runtime } }
@safe unittest
{
    struct C
    {
        ubyte i;
        this(ubyte i) { this.i = i; }
    }

    auto c1 = C(1);
    auto c2 = C(2);

    assert(__cmp([c1, c1][], [c2, c2][]) < 0);
    assert(__cmp([c2, c2], [c1, c1]) > 0);
    assert(__cmp([c2, c2], [c2, c1]) > 0);
}

@safe unittest
{
    struct C
    {
        char i;
        this(char i) { this.i = i; }
    }

    auto c1 = C(1);
    auto c2 = C(2);

    assert(__cmp([c1, c1][], [c2, c2][]) < 0);
    assert(__cmp([c2, c2], [c1, c1]) > 0);
    assert(__cmp([c2, c2], [c2, c1]) > 0);
}
