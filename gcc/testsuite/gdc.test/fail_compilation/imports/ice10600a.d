module imports.ice10600a;

struct Appender(A : T[], T)
{
    this(T[]) {}
}

Appender!(E[]) appender(A : E[], E)()
{
    return Appender!(E[])(null);
}
