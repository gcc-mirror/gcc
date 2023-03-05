// REQUIRED_ARGS: -o-
// PERMUTE_ARGS:

class Foo
{
    void foo()
    {
        foreach (f; __traits(getOverloads, typeof(this), "bar"))
        {
            auto dg = &f;
        }

        foreach (f; __traits(getVirtualMethods, typeof(this), "bar"))
        {
            auto dg = &f;
        }
    }

    uint bar() { return 0; }
}
