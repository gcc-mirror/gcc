void fn(int* x, real[1] arr)
{
    auto y = *x;  // should not segfault
    assert(y == 42, "x parameter corrupted");
    assert(arr[0] == 1.0, "arr (real[1]) corrupted");
}

void main()
{
    real[1] arr = [1.0];
    int x = 42;
    fn(&x, arr);

    assert(x == 42, "x value corrupted");
    assert(arr[0] == 1.0, "arr (real[1]) value corrupted");
}
