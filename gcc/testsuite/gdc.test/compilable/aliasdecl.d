template Test(T){ alias Type = T; }

alias X1 = int;
static assert(is(X1 == int));

alias X2 = immutable(long)[], X3 = shared const double[int];
static assert(is(X2 == immutable(long)[]));
static assert(is(X3 == shared const double[int]));

alias X4 = void delegate() const, X5 = Test!int;
static assert(is(X4 == void delegate() const));
static assert(is(X5.Type == int));

alias FP5 = extern(C) pure nothrow @safe @nogc void function(),
      DG5 = extern(D) pure nothrow @safe @nogc void delegate();
static assert(FP5.stringof == "extern (C) void function() pure nothrow " /* ~ "@safe " */ ~ "@nogc");
static assert(DG5.stringof ==            "void delegate() pure nothrow " /* ~ "@safe " */ ~ "@nogc");

void main()
{
    alias Y1 = int;
    static assert(is(Y1 == int));

    alias Y2 = immutable(long)[], Y3 = shared const double[int];
    static assert(is(Y2 == immutable(long)[]));
    static assert(is(Y3 == shared const double[int]));

    alias Y4 = void delegate() const, Y5 = Test!int;
    static assert(is(Y4 == void delegate() const));
    static assert(is(Y5.Type == int));

    // https://issues.dlang.org/show_bug.cgi?id=18429
    struct S
    {
        alias a this;
        enum a = 1;
    }

    struct S2
    {
        int value;
        alias this = value;
    }
    auto s = S2(10);
    int n = s;
    assert(n == 10);
}
