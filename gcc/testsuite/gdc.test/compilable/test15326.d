// REQUIRED_ARGS: -w -c -unittest

version (unittest)
private struct _NestedSym_
{
    static if ((void*).sizeof == 8)
    {
        pragma(msg, "64");
    }
    else
    {
        pragma(msg, "32");
    }

    version (X86_64)
    {
        pragma(msg, "X86_64");
    }
    else
    {
        pragma(msg, "Not 64");
    }
}
