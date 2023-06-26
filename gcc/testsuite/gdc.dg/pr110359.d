// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=110359
// { dg-do compile }
// { dg-options "-fdump-tree-original" }
double pow(in double x, in ulong p)
{
    import gcc.builtins : __builtin_expect;
    if (__builtin_expect(p == 0, false))
        return 1;
    if (__builtin_expect(p == 1, false))
        return x;

    double s = x;
    double v = 1;
    for (ulong i = p; i > 1; i >>= 1)
    {
        v = (i & 0x1) ? s * v : v;
        s = s * s;
    }
    return v * s;
}
// { dg-final { scan-tree-dump "if \\(__builtin_expect \\(p == 0, 0\\) != 0\\)" "original" } }
// { dg-final { scan-tree-dump "if \\(__builtin_expect \\(p == 1, 0\\) != 0\\)" "original" } }
