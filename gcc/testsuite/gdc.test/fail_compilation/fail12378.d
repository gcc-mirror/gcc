/*
TEST_OUTPUT:
---
fail_compilation/fail12378.d(18): Error: undefined identifier `ANYTHING`
fail_compilation/fail12378.d(18): Error: undefined identifier `GOES`
fail_compilation/fail12378.d(91):        instantiated from here: MapResultS!((x0) => ANYTHING - GOES, Result)
fail_compilation/fail12378.d(17):        instantiated from here: mapS!(Result)
fail_compilation/fail12378.d(100):        instantiated from here: __lambda1!int
fail_compilation/fail12378.d(91):        instantiated from here: MapResultS!((y0) => iota(2).mapS!((x0) => ANYTHING - GOES), Result)
fail_compilation/fail12378.d(16):        instantiated from here: mapS!(Result)
---
*/
void testS()
{
    auto r =
    iota(1).mapS!(y0 =>
        iota(2).mapS!(x0 =>
            ANYTHING-GOES
        )
    );
}

/*
TEST_OUTPUT:
---
fail_compilation/fail12378.d(40): Error: undefined identifier `ANYTHING`
fail_compilation/fail12378.d(40): Error: undefined identifier `GOES`
fail_compilation/fail12378.d(112):        instantiated from here: MapResultC!((x0) => ANYTHING - GOES, Result)
fail_compilation/fail12378.d(39):        instantiated from here: mapC!(Result)
fail_compilation/fail12378.d(123):        instantiated from here: __lambda1!int
fail_compilation/fail12378.d(112):        instantiated from here: MapResultC!((y0) => iota(2).mapC!((x0) => ANYTHING - GOES), Result)
fail_compilation/fail12378.d(38):        instantiated from here: mapC!(Result)
---
*/
void testC()
{
    auto r =
    iota(1).mapC!(y0 =>
        iota(2).mapC!(x0 =>
            ANYTHING-GOES
        )
    );
}

/*
TEST_OUTPUT:
---
fail_compilation/fail12378.d(64): Error: undefined identifier `ANYTHING`
fail_compilation/fail12378.d(64): Error: undefined identifier `GOES`
fail_compilation/fail12378.d(135):        instantiated from here: MapResultI!((x0) => ANYTHING - GOES, Result)
fail_compilation/fail12378.d(63):        instantiated from here: mapI!(Result)
fail_compilation/fail12378.d(143):        instantiated from here: __lambda1!int
fail_compilation/fail12378.d(135):        instantiated from here: MapResultI!((y0) => iota(2).mapI!((x0) => ANYTHING - GOES), Result)
fail_compilation/fail12378.d(62):        instantiated from here: mapI!(Result)
---
*/


void testI()
{
    auto r =
    iota(1).mapI!(y0 =>
        iota(2).mapI!(x0 =>
            ANYTHING-GOES
        )
    );
}

auto iota(E)(E end)
{
    alias Value = E;

    static struct Result
    {
        private Value current, pastLast;

        @property inout(Value) front() inout { return current; }
    }

    return Result(0, end);
}

template mapS(fun...)
{
    auto mapS(R)(R r)
    {
        alias AppliedReturnType(alias f) = typeof(f(r.front));
        static assert(!is(AppliedReturnType!fun == void),
            "Mapping function must not return void.");

        return MapResultS!(fun, R)(r);
    }
}
struct MapResultS(alias fun, R)
{
    R _input;

    @property auto ref front()
    {
        return fun(_input.front);
    }
}

template mapC(fun...)
{
    auto mapC(R)(R r)
    {
        alias AppliedReturnType(alias f) = typeof(f(r.front));
        static assert(!is(AppliedReturnType!fun == void),
            "Mapping function must not return void.");

        return new MapResultC!(fun, R)(r);
    }
}
class MapResultC(alias fun, R)
{
    R _input;

    this(R r) { _input = r; }

    @property auto ref front()
    {
        return fun(_input.front);
    }
}

template mapI(fun...)
{
    auto mapI(R)(R r)
    {
        alias AppliedReturnType(alias f) = typeof(f(r.front));
        static assert(!is(AppliedReturnType!fun == void),
            "Mapping function must not return void.");

        return MapResultI!(fun, R).init;
    }
}
interface MapResultI(alias fun, R)
{
    static @property auto ref front()
    {
        R _input;
        return fun(_input.front);
    }
}
