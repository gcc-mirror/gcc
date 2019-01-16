// PREMUTE_ARGS:
struct S9613
{
    int f(
        const(byte) a = const(byte).init,
        immutable(byte) b = immutable(byte).init,
        shared(byte) c = shared(byte).init,
        inout(byte) d = inout(byte).init,
    ) inout
    {
        assert(a == byte.init);
        assert(b == byte.init);
        assert(c == byte.init);
        assert(d == byte.init);
        static assert(const(byte).init == byte.init);
        static assert(immutable(byte).init == byte.init);
        static assert(shared(byte).init == byte.init);
        static assert(inout(byte).init == byte.init);
        return 0;
    }
}

void main()
{
    static assert(const(byte).init == byte.init);
    static assert(immutable(byte).init == byte.init);
    static assert(shared(byte).init == byte.init);
    static assert(const(byte).init.sizeof == byte.sizeof);
    static assert(const(byte[2]).init[0] == byte.init);
    enum s = S9613();
    enum v = s.f();
}
