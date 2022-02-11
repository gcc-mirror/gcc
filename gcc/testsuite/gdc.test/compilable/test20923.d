version (D_LP64)
{
    alias size_t = uint;
}
else
{
    alias size_t = ulong;
}

struct S
{
    real not_reproduceable_without_this_variable;
}
