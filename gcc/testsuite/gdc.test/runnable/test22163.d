// https://issues.dlang.org/show_bug.cgi?id=22163
void fun(float[2] arr)
{
    assert(arr[0] == 10.0);
    assert(arr[1] == 20.0);
    auto dg = (int x) => arr[0];
}

void main()
{
    float[2] arr = [10.0, 20.0];
    fun(arr);
}
