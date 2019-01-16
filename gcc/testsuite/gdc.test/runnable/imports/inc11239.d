// REQUIRED_ARGS:
module imports.inc11239;

int foo(T)(T x)
{
    return 3;
}

debug
{
    int x = foo(2);
}
