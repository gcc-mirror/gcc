// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=94777
// { dg-additional-options "-fmain -funittest" }
// { dg-do run }
// { dg-skip-if "needs gcc/config.d" { ! d_runtime } }

void testVariadic(T)(int nargs, ...)
{
    import core.stdc.stdarg;
    foreach(i; 0 .. nargs)
    {
        auto arg = va_arg!T(_argptr);
        static if (__traits(compiles, arg.value))
        {
            assert(arg.value == i);
        }
        else static if (__traits(compiles, arg[0]))
        {
            foreach (value; arg)
                assert(value == i);
        }
        else
        {
            assert(arg == T.init);
        }
    }
}

/******************************************/

struct Destructor
{
    static int count = 0;
    int value;
    ~this() { count++; }
}

unittest
{
    {
        auto a0 = Destructor(0);
        auto a1 = Destructor(1);
        auto a2 = Destructor(2);
        static assert(!__traits(compiles, testVariadic!Destructor(3, a0, a1, a2)));
    }
    assert(Destructor.count == 3);
}
