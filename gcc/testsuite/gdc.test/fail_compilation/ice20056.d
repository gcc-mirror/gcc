/*
TEST_OUTPUT:
---
fail_compilation/ice20056.d(19): Error: calling non-static function `iter` requires an instance of type `RangeWrapper`
---
*/
struct Def(alias fn)
{
    alias func = alias_selector!(fn).VOverloads[0];
}

template alias_selector(alias fn)
{
    alias VOverloads = __traits(getOverloads, __traits(parent, fn), __traits(identifier, fn));
}

void init_rangewrapper()
{
    Def!(RangeWrapper.iter).func;
}

struct RangeWrapper
{
    void iter() { }
}
