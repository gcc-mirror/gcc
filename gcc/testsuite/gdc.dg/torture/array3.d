// { dg-do run }
// { dg-skip-if "needs gcc/config.d" { ! d_runtime } }

void foo13988(double[] arr)
{
    static ulong repr(double d) { return *cast(ulong*)&d; }
    foreach (x; arr)
        assert(repr(arr[0]) == *cast(ulong*)&(arr[0]));
}

void main()
{
    double[] arr = [3.0];
    foo13988(arr);
}
