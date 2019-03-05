// REQUIRED_ARGS:
// PERMUTE_ARGS:

union U
{
    bool a;
    long b;
}

U test1()
{
    return U();
}

U* test2()
{
    return new U();
}
