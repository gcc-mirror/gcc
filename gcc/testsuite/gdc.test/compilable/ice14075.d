// REQUIRED_ARGS: -o-
// PERMUTE_ARGS:

struct Foo
{
    auto opAssign(this X)(ref typeof(this));
    auto opAssign(this X, V)(ref V) if (!is(V == typeof(this)));
}

void test()
{
    Foo src;
    const(Foo) target;
    static if (is(typeof(target = src))) {}
}
