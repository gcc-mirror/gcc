/*
TEST_OUTPUT:
---
fail_compilation/ice12574.d(40): Error: sequence index `2` out of bounds `[0 .. 2]`
fail_compilation/ice12574.d(53): Error: template instance `ice12574.reduce!("a", "a").reduce!(Tuple!(int, int, int))` error instantiating
---
*/

struct Tuple(T...)
{
    alias Types = T;
    T field;
    alias field this;
}
Tuple!A tuple(A...)(A args) { return typeof(return)(args); }

template binaryFun(alias fun)
{
    static if (is(typeof(fun) : string))
    {
        auto binaryFun(ElementType1, ElementType2)(auto ref ElementType1 __a, auto ref ElementType2 __b)
        {
            mixin("alias "~"a"~" = __a ;");
            mixin("alias "~"b"~" = __b ;");
            return mixin(fun);
        }
    }
    else
    {
        alias binaryFun = fun;
    }
}

template reduce(fun...)
{
    auto reduce(Seed)(Seed result)
    {
        foreach (i, Unused; Seed.Types)
        {
           result[i] = binaryFun!(fun[i])(1, 1); // here
        }
        return result;
    }
}

int foo(int value)
{
    return value;
}

void main()
{
    reduce!("a", "a")(tuple(1, 1, 1));
}
