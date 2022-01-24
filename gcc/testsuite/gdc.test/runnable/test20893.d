// REQUIRED_ARGS: -O -inline
//
// https://issues.dlang.org/show_bug.cgi?id=20893
// caused by https://github.com/dlang/dmd/pull/9722

int f(int n)
{
    foreach (i; 0..n) {}
    return 10;
}

int c(int a, int b)
{
    return (f(a) * 1L * f(b)) % 1000;
}

void main()
{
    int[] a = [1];
    assert(c(2 - 1, 2) == 100);
}
