// REQUIRED_ARGS: -o-
// PERMUTE_ARGS:

void main() pure
{
    int[] foo;

    // if indirectly instantiated aggregate is struct (== MapResultS)
    foo.map!(MapResultS, x => foo.map!(MapResultS, y => x).array);

    // if indirectly instantiated aggregate is class (== MapResultC)
    foo.map!(MapResultC, x => foo.map!(MapResultC, y => x).array);
}

T array(T)(T a)
{
    static int g; g = 1;    // impure operation
    return a;
}

template map(alias MapResult, fun...)
{
    auto map(Range)(Range r)
    {
        alias AppliedReturnType(alias f) = typeof(f(r[0]));
        static assert(!is(AppliedReturnType!fun == void));

        return MapResult!(fun).init;
    }
}

struct MapResultS(alias fun)
{
    @property front()
    {
        return fun(1);
    }
}

class MapResultC(alias fun)
{
    @property front()
    {
        return fun(1);
    }
}
