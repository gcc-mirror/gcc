// COMPILED_IMPORTS: imports/b3682.d
// PERMUTE_ARGS:

// https://issues.dlang.org/show_bug.cgi?id=3682

struct Tuple(Types...)
{
    Tuple!(Types[0..1]) slice()()
    {
        Tuple!(Types[0..1]) x;
        return x;
    }

    void fail()
    {
        Tuple!(float, double, int) a;
        auto s = a.slice();
        static assert(is(typeof(s) == Tuple!(float)));
    }
}
