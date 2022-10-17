module test18545;

enum Constness
{
    Mutable,
    Const,
    Immutable,
}

enum Cases = [Constness.Mutable, Constness.Const, Constness.Immutable];

void main()
{
    static foreach (from; Cases)
    {
        static foreach (to; Cases)
        {
            test!(from, to)();
        }
    }
}

void test(Constness from, Constness to)()
{
    struct S {
        int i;

        @property int get() const { return 0; }

        alias get this;
    }

    static if (from == Constness.Mutable)
    {
        alias ConstS = S;
    }
    else static if (from == Constness.Const)
    {
        alias ConstS = const(S);
    }
    else
    {
        alias ConstS = immutable(S);
    }

    ConstS s1 = S(42);

    // this should reinterpret-cast, NOT call the implicit constructor with .get!
    static if (to == Constness.Mutable)
    {
        auto s2 = cast() s1;
    }
    else static if (to == Constness.Const)
    {
        const s2 = cast(const) s1;
    }
    else static if (to == Constness.Immutable)
    {
        immutable s2 = cast(immutable) s1;
    }

    assert(s2.i == s1.i, "Bug 18545 occurred casting from "~from.stringof~" to "~to.stringof);
}
