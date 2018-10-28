// REQUIRED_ARGS: -o-
// PERMUTE_ARGS: -w

int foo(Args...)()
{
    int x;

    foreach (arg; Args)
    {
        static if(is(arg == int))
        {
            return 0;
        }
        static if(is(arg == long))
        {
            // fallthrough
            ++x;    // this statement might be unreachable, but
                    // UnrollStatement does not warn that.
        }
    }
    // no return
}

void main()
{
    auto r1 = foo!(int)();          // return
    auto r2 = foo!(int, long)();    // return -> fallthrough (it's unreachable)
    auto r3 = foo!(long, int)();    // fallthough -> return
    static assert(!__traits(compiles, foo!(long)()));       // fallthough
    static assert(!__traits(compiles, foo!(long, long)())); // fallthough -> fallthough
}
