// https://issues.dlang.org/show_bug.cgi?id=20021
struct S
{
    bool opCast(T : bool)() { return true; }
    S prop() {return this;}
    S prop(S newThis) {return this;}
}

struct T
{
    bool opCast(T : bool)() { return false; }
}

void test20021()
{
    static if (T.init)
        static assert(false);

    // ensure properties are resolved
    static if (!(true && T.init || (S.init.prop = S.init).prop))
        static assert(false);
}
