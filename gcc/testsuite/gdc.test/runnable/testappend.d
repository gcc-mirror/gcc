/*
PERMUTE_ARGS:
RUN_OUTPUT:
---
Success
---
*/

import core.stdc.stdio;
import core.stdc.math : isnan;

void test12826()
{
    string s, t;
    t = t ~ "1234567";
    s = s ~ "1234567";

    s ~= s;
    assert(s == "12345671234567", s);
    assert(t == "1234567", t);
}


int main()
{
    int[] a;

    for (int i = 0; i < 1000; i++)
    {
        a.length = a.length + 100;
    }
    foreach (v; a)
    {
        assert(v == 0);
    }

    float[] b;
    for (int i = 0; i < 2000; i++)
    {
        b.length = b.length + 100;
    }
    foreach (v; b)
    {
        assert(isnan(v));
    }
    destroy(a);
    destroy(b);

    a = null;
    for (int i = 0; i < 100000; i++)
    {
        a ~= i;
    }
    foreach (k, v; a)
    {
        assert(v == k);
    }

    b = null;
    for (int i = 0; i < 200000; i++)
    {
        b ~= i;
    }
    foreach (k, v; b)
    {
        assert(v == k);
    }
    destroy(a);
    destroy(b);

    test12826();
    printf("Success\n");
    return 0;
}
