// https://github.com/dlang/dmd/issues/21894

inout(int[]) test1(inout(int[]) a, int b)
{
    return a ~ b;
}
inout(int[]) test2(inout(int[]) a, inout(int[]) b)
{
    return a ~ b;
}
void main()
{
    test1([1], 2);
    test2([1], [2]);
}
