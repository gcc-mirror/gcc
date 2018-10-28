// EXTRA_SOURCES: imports/a7595.d

template isSafe(alias func)
{
    @safe void dummySafeFunc()
    {
        func();
    }

    enum isSafe = is(typeof(dummySafeFunc()));
}

template areAllSafe(funcs...)
{
    enum areAllSafe = isSafe!(funcs[0]);
}

@safe benchmark(fun...)(uint n)
if (areAllSafe!fun)
{
    foreach(i, unused; fun)
    {
        foreach (j; 0 .. n)
            fun[i]();
    }
}
