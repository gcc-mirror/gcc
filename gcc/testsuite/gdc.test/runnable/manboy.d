// PERMUTE_ARGS:

import std.stdio;

int a(int k, lazy int x1, lazy int x2, lazy int x3, lazy int x4, lazy int x5)
{
    int delegate() b;
    b = { k -= 1; return a(k, b(), x1, x2, x3, x4); };
    if (k <= 0)
        return x4 + x5;
    else
        return b();
}

int main()
{
    assert(a(10, 1, -1, -1, 1, 0) == -67);
    return 0;
}

