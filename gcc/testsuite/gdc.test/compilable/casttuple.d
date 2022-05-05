alias tuple(T...) = T;

void exactMatch()
{
    tuple!int tup_1;

    auto i = cast() tup_1;
    static assert(is(typeof(i) == int));
    const i_const = cast(const) tup_1;
    static assert(is(typeof(i_const) == const int));

    auto totup_1 = cast(tuple!int) tup_1;
    static assert(is(typeof(totup_1) == tuple!int));

    tuple!(int, int) tup_2;
    auto totup_2 = cast(tuple!(int, int)) tup_2;
    static assert(is(typeof(totup_2) == tuple!(int, int)));
}

void implicitConv()
{
    tuple!short tup_1;
    auto totup_1 = cast(tuple!int) tup_1;
    static assert(is(typeof(tup_1) == tuple!short));
    static assert(is(typeof(totup_1) == tuple!int));

    tuple!(short, short) tup_2;
    auto totup_2 = cast(tuple!(int, int)) tup_2;
    static assert(is(typeof(tup_2) == tuple!(short, short)));
    static assert(is(typeof(totup_2) == tuple!(int, int)));
}
