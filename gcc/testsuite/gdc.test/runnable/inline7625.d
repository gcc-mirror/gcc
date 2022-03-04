// REQUIRED_ARGS: -inline

/***************************************************/

pragma(inline, true)
{
    int foo1(int v)
    {
        return bar1(2 * v);
    }

    int bar1(int a)
    {
        if (a > 0)
            return 1;
        else
            return baz1(a);
    }
}

int baz1(int a)
{
    if (a > 0)
        throw new Exception("a > 0");
    return a - 1;
}

// ---

pragma(inline, true)
{
    int foo2(int v)
    {
        return bar2(2 * v);
    }

    int bar2(int a)
    {
        if (a > 0)
            return 1;
        // else
            return baz2(a);
    }
}

int baz2(int a)
{
    if (a > 0)
        throw new Exception("a > 0");
    return a - 1;
}

// ---

pragma(inline, true)
{
    int foo3(int v)
    {
        return bar3(2 * v);
    }

    int bar3(int a)
    {
        if (a > 0)
            a = 1;
        else
            return baz3(a);
        return a;
    }
}

int baz3(int a)
{
    if (a > 0)
        throw new Exception("a > 0");
    return a - 1;
}

void test7625a()
{
    assert(foo1(1) ==  1);
    assert(foo1(0) == -1);
    assert(foo2(1) ==  1);
    assert(foo2(0) == -1);
    assert(foo3(1) ==  1);
    assert(foo3(0) == -1);
}

/***************************************************/

@safe pragma(inline, true)
{
    int tembo(int x, int y)
    {
        if (y == 0)
            return 0;
        x++;
        return x / y;
    }
    int pembo(int x, int y)
    {
        if (y == 0)
            return 0;
        else
        {
            x++;
            return x / y;
        }
    }

    int twiga(int x, int y, int z)
    {
        auto w = tembo(x, y);
        return w * z;
    }

    int simba(int x, int y, int z)
    {
        auto w = pembo(x, y);
        return w * z;
    }
}

void test7625b()
{
    assert(twiga(5, 3, 4) == 8);
    assert(twiga(5, 0, 4) == 0);

    assert(simba(5, 3, 4) == 8);
    assert(simba(5, 0, 4) == 0);
}

/***************************************************/

@safe pragma(inline, true)
{
    bool inlineMe15483a(bool left)
    {
        if (left)
            return true;

        return false;
    }

    bool inlineMe15483b(bool left)
    {
        if (left)
            return true;

        static if (false)
        {
            /* https://issues.dlang.org/show_bug.cgi?id=15483
               Even though it does absolutely nothing,
               the mere presence of this block prevents inlining
               of this function.
            */
        }

        return false;
    }
}

int foo15483()
{
    auto r1 = inlineMe15483a(true);
    auto r2 = inlineMe15483b(true); // OK <- NG

    return 128;
}

void test15483()
{
    // Prevent inlining of test function call.
    auto fp = &foo15483;
    assert(fp() == 128);
}

/***************************************************/

void main()
{
    test7625a();
    test7625b();
    test15483();
}
