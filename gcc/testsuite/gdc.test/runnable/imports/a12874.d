module imports.a12874;

template foo(alias x)
{
    void check(int[] arr)
    {
        auto n = arr[0];
    }
    void foo()
    {
        check([]);
    }
}
