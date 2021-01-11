// REQUIRED_ARGS: -w -c -unittest

version (unittest)
private struct _NestedSym_
{
    static if ((void*).sizeof == 8)
    {
        int pointersize = 64;
    }
    else
    {
        int pointersize = 32;
    }

    version (X86_64)
    {
        string arch = "X86_64";
    }
    else
    {
        string arch = "Not 64";
    }
}
