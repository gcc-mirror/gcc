// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=94777
// { dg-additional-options "-funittest" }
// { dg-do compile }

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

struct Postblit
{
    static int count = 0;
    int value;
    this(this) { count++; }
}

unittest
{
    auto a0 = Postblit(0);
    auto a1 = Postblit(1);
    auto a2 = Postblit(2);
    testVariadic!Postblit(3, a0, a1, a2); // { dg-error "cannot pass types with postblits or copy constructors as variadic arguments" }
    assert(Postblit.count == 3);
}

/******************************************/

struct CopyConstructor 
{
    static int count = 0;
    int value;
    this(int v) { this.value = v; }
    this(ref typeof(this) other) { count++; this.value = other.value; }
}

unittest
{
    auto a0 = CopyConstructor(0);
    auto a1 = CopyConstructor(1);
    auto a2 = CopyConstructor(2);
    testVariadic!CopyConstructor(3, a0, a1, a2); // { dg-error "cannot pass types with postblits or copy constructors as variadic arguments" }
    assert(CopyConstructor.count == 3);
}
