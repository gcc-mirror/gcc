// REQUIRED_ARGS: -o-
// PERMUTE_ARGS:

T foo(T)()
{
    __gshared int[] bar = [];
    return T.init;
}

void main()
{
    foo!char();
}
