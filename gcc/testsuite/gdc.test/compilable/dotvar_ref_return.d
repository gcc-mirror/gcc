// REQUIRED_ARGS: -inline

struct Foo
{
    int x;
    int foo()
    {
        ref get() { return x; }
        return get();
    }
}
