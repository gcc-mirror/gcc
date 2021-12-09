// https://issues.dlang.org/show_bug.cgi?id=20051
struct Templ2(Args...)
{
}

struct WillAlsoWork(alias T : Templ!Args, Args...)
{
    alias A = Args[0];
}

void main()
{
    alias C2 = Templ2!int;
    static assert(!__traits(compiles, {
        alias B2 = WillAlsoWork!C2;
        B2.A a2;
    }));
}
