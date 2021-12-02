/*
TEST_OUTPUT:
---
fail_compilation/diag14145.d(15): Error: no property `i` for type `diag14145.main.Capture!(i)`
fail_compilation/diag14145.d(15):        potentially malformed `opDispatch`. Use an explicit instantiation to get a better error message
fail_compilation/diag14145.d(34): Error: expression `*this.ptr` of type `shared(int)` is not implicitly convertible to return type `ref int`
fail_compilation/diag14145.d(16): Error: template instance `diag14145.main.Capture!(i).Capture.opDispatch!"i"` error instantiating
---
*/

int main()
{
    int i;
    auto _ = capture!i;
    _.i;
    _.opDispatch!"i";
    return 0;
}

auto capture(alias c)()
{
    return Capture!c(c);
}

struct Capture(alias c)
{
    shared typeof(c)* ptr;
    this(ref typeof(c) _c)
    {
        ptr = cast(shared)&c;
    }
    ref shared typeof(c) opDispatch(string s)()
    {
        return *ptr;
    }
}


