// PERMUTE_ARGS: -inline -release -g -O -d -dw -de

void test(int*[] arr...)
{
    assert(arr.length == 1);
    assert(*arr[0] == 5); // This assertion fails
}

void main()
{
    int a = 5;
    test([&a]);
}
