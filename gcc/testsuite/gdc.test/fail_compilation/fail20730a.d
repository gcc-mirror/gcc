/*
REQUIRED_ARGS: -o-
TEST_OUTPUT:
---
fail_compilation/fail20730a.d(11): Error: undefined identifier `undef20730`
---
*/
void test20730()
{
    auto f = File().byLine;
    undef20730();
}

struct File
{
    shared uint refs;

    this(this)
    {
        atomicOp!"+="(refs, 1);
    }

    struct ByLineImpl(Char)
    {
        File file;
        char[] line;
    }

    auto byLine()
    {
        return ByLineImpl!char();
    }
}

T atomicOp(string op, T, V1)(ref shared T val, V1 mod)
    if (__traits(compiles, mixin("*cast(T*)&val" ~ op ~ "mod")))
{
    return val;
}
