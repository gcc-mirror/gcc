inout(int) foo(inout(int) x)
{
    x = 5;  // cannot modify inout
    return 0;
}
