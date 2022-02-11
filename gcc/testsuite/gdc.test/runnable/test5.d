// PERMUTE_ARGS:

extern(C) int printf(const char*, ...);

interface foo
{
    extern (C) int testc(int i);
    extern (Windows) int testw(int i);
    extern (D) int testd(int i);
}

class bar : foo
{
    int x = 47;

  extern (C)
    int testc(int i)
    {
        printf("foo.testc(%p)\n", this);
        assert(x == 47);
        return i + x;
    }

  extern (Windows)
    int testw(int i)
    {
        printf("foo.testw(%p)\n", this);
        assert(x == 47);
        return i + x;
    }

  extern (D)
    int testd(int i)
    {
        printf("foo.testd(%p)\n", this);
        assert(x == 47);
        return i + x;
    }
}

int def(foo f)
{
    printf("def(%p), %d\n", f, (cast(int*)f)[0]);
    assert(f.testc(3) == 50);
    assert(f.testd(7) == 54);
    assert(f.testd(10) == 57);
    return 0;
}

void abc(bar b)
{
    printf("abc(%p), %d\n", b, (cast(int*)b)[3]);
    def(b);
}

int main()
{
    bar b = new bar();

    printf("b.size = x%zx\n", b.classinfo.initializer.length);
    printf("bar.size = x%zx\n", bar.classinfo.initializer.length);
    assert(b.classinfo.initializer.length == bar.classinfo.initializer.length);
    abc(b);
    return 0;
}
